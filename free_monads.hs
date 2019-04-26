
--- v1

data BillingState
    = BillingState
    { userId :: UserId
    , userBalance :: Double
    , userSubscription :: SubscriptionId
    , lastPaymentDate :: Day
    }

data BillingProgram
    = GetUserBalance
    | GetUserLastPaymentDate
    | CancelSubscription
    | ChargeUser
    | SendLateNotice

interpret :: BillingProgram -> StateT BillingState IO ()
interpret GetUserBalance = do
    id <- gets userId
    balance <- liftIO $ Stripe.getUserBalance id
    modify (\s -> s { userBalance = balance })



--- v2

data BillingProgram
    = GetUserBalance UserId
    | GetUserLastPaymentDate UserId
    | CancelSubscription UserId PlanId
    | ChargeUser UserId Double
    | SendLateNotice PlanId Email

interpret :: BillingProgram -> IO ()
interpret (GetUserBalance userId) =
    Stripe.getUserBalance userId
interpret (GetUserLastPaymentDate userId) =
    Stripe.getLastPaymentDate userId
interpret (CancelSubscription userId planId) = do
    subscriptions <- Stripe.getSubscriptionsFor userId
    for_ subscriptions $ \sub -> do
        when (sunPlan sun == planId) $ do
            Stripe.cancelSubscription (subId sub)



--- v3

data BillingProgram
    = GetUserBalance UserId            (Double -> BillingProgram)
    | GetUserLastPaymentDate UserId    (Day -> BillingProgram)
    | CancelSubscription UserId PlanId BillingProgram
    | ChargeUser UserId Double         BillingProgram
    | SendLateNotice PlanId Email      BillingProgram

data BillingProgram
    = GetUserBalance UserId            (Double -> BillingProgram)
    | GetUserLastPaymentDate UserId    (Day -> BillingProgram)
    | CancelSubscription UserId PlanId BillingProgram
    | ChargeUser UserId Double         BillingProgram
    | SendLateNotice PlanId Email      BillingProgram
    | Done

chargeOrEmail :: User -> Subscription -> BillingProgram
chargeOrEmail user sub =
    GetUserBalance (userId user) $ \userBalance ->
        if userBalance >= subPrice sub
           then ChargeUser (userId user) (subPrice sub) Done
           else SendLateNotice (subPlan sub) (userEmail user) Done



--- v4

data BillingProgram ret
    = GetUserBalance UserId            (Double -> BillingProgram ret)
    | GetUserLastPaymentDate UserId    (Day -> BillingProgram ret)
    | CancelSubscription UserId PlanId (BillingProgram ret)
    | ChargeUser UserId Double         (BillingProgram ret)
    | SendLateNotice PlanId Email      (BillingProgram ret)
    | Done ret

chargeOrEmail :: User -> Subscription -> BillingProgram Bool
chargeOrEmail user sub =
    GetUserBalance (userId user) $ \userBalance ->
        if userBalance >= subPrice sub
           then ChargeUser (userId user) (subPrice sub) (Done True)
           else SendLateNotice (subPlan sub) (userEmail user) (Done False)



--- v5

andThen
    :: BillingProgram a
    -> (a -> BillingProgram b)
    -> BillingProgram b
andThen (Done ret) mkProgram = mkProgram ret
andThen (GetUserBalance userId next) mkProgram =
    GetUserBalance userId (\balance -> andThen (next balance) mkProgram)
andThen (GetUserLastPaymentDate userId next) mkProgram =
    GetUserLastPaymentDate userId (\date -> andThen (next date) mkProgram)
andThen (CancelSubscription userId planId next) =
    CancelSubscription userId planId (andThen next mkProgram)
andThen (ChargeUser userId amount next) =
    ChargeUser userId amount (andThen next mkProgram)
andThen (SendLateNotice planId email next) =
    SendLateNotice planId notice (andThen next mkProgram)

getUserBalance :: UserId -> BillingProgram Double
getUserBalance userId =
    GetUserBalance userId (\amount -> Done amount)

end :: BillingProgram ()
end = Done ()

getLastPaymentDate :: UserId -> BillingProgram Day
getLastPaymentDate userId =
    GetUserLastPaymentDate userId (\day -> Done day)

cancelSubscription :: UserId -> PlanId -> BillingProgram ()
cancelSubscription userId planId =
    CancelSubscription userId planId end


billingProgram :: User -> [Subscription] -> BillingProgram ()
billingProgram _ [] =
    end
billingProgram user (sub:subs) =
    getUserBalance uid `andThen` \balance ->
    if balance > price then
        chargeUser uid price
            `andThen` \_ -> theRest
    else
        sendLateNotice plan (userEmail user) `andThen` \_ ->
        getUserLastPaymentDate uid `andThen` \day ->
        if day < 60daysago
           then cancelSubscription uid plan
                    `andThen` \_ -> theRest
           else theRest
  where
    uid = userId user
    price = subPrice sub
    plan = subPlan sub
    theRest = billingProgram user subs



--- v6

instance Monad BillingProgram where
    return = Done
    (>>=) = andThen

-- forM_ :: (Monad m) => [a] -> (a -> m b) -> m ()
-- when :: (Monad m) => Bool -> m () -> m ()

billingProgram :: User -> [Subscription] -> BillingProgram ()
billingProgram user subs = forM_ subs $ \sub -> do
    let uid = userId user
        price = subPrice sub
        plan = subPlan sub
    balance <- getUserBalance uid
    if balance > price
        then do
            chargeUser uid price
        else do
            day <- getUserLastPaymentDate uid
            when (day < 60daysago) $ do
                cancelSubscription uid plan


interpret :: BillingProgram a -> IO a
interpret (Done a) = pure a
interpret (ChargeUser uid price next) = do
    Stripe.chargeUser uid price
    interpret next
interpret (SendLateNotice plan email next) = do
    Email.sendLateNoticeFor plan email
    interpret next
interpret (GetUserBalance uid next) = do
    balance <- Stripe.getBalance uid
    interpret (next balance)
interpret etcccc = do
    putStrLn "you could finish me"



--- testing

interpretTest :: BillingProgram a -> State Mock a
interpretTest (Done a) = pure a
interpretTest (ChargeUser uid price next) = do
    modify (subtractBalance uid price)
    interpret next
interpretTest (SendLateNotice plan email next) = do
    modify (addBillingEmail plan email)
    interpret next
interpretTest (GetUserBalance uid next) = do
    balance <- gets (userBalance uid)
    interpret (next balance)
interpretTest etc = error "finish meeee"



--- v7

data Free f a
    = Free (f (Free f a))
    | Done a

-- data List   a
--     = Cons a (List a)
--     | Nil

data BillingF next
    = GetUserBalance UserId (Double -> next)
    | ChargeUser UserId Double next
    | Etc

type Billing = Free BillingF

getUserBalance :: UserId -> Billing Double
getUserBalance userId = Free (GetUserBalance userId Done)

chargeUser :: UserId -> Double -> Billing ()
chargeUser uid amt = Free (ChargeUser uid amt (Done ()))



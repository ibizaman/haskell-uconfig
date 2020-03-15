{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-|
Module      : Config.SystemdService
Description : Systemd service file parser
Copyright   : (c) Pierre Penninckx, 2020
License     : GPL-3
Maintainer  : Pierre Penninckx (ibizapeanut@gmail.com)
Stability   : experimental
Portability : POSIX

The Config.SystemdService module provides a function to parse Systemd
Service files.

https://www.freedesktop.org/software/systemd/man/systemd.service.html
-}
module Config.SystemdService
    ( EmptyDefault(..)
    , SystemdService(..)
    , Unit(..)
    , Description(..)
    , Documentation(..)
    , Target(..)
    , Install(..)
    , Service(..)
    , Type(..)
    , Exec(..)
    , User(..)
    , Group(..)
    , PIDFile(..)
    , RemainAfterExit(..)
    , BusName(..)
    , NotifyAccess(..)
    , WorkingDirectory(..)
    , Output(..)
    , TasksMax(..)
    , PrivateTmp(..)
    , Restart(..)
    )
where


import           Control.Applicative            ( (<|>) )
import           Data.Functor                   ( ($>) )
import qualified Data.Text                     as T
import           Data.String                    ( IsString )
import qualified Parser                        as P

import qualified Config                        as C


-- |Value that can be empty. It's Maybe with a different Semigroup
-- implementation.
data EmptyDefault a = Empty | Value a
  deriving(Eq, Show)

instance Semigroup (EmptyDefault x) where
    Value a <> _       = Value a
    _       <> Value b = Value b
    _       <> _       = Empty

instance Monoid (EmptyDefault x) where
    mempty = Empty

instance Functor EmptyDefault where
    fmap f  (Value m) = Value (f m)
    fmap _f Empty     = Empty

instance Applicative EmptyDefault where
    pure = Value
    Value f <*> m  = fmap f m
    Empty   <*> _m = Empty

instance (C.Config a) => C.Config (EmptyDefault a) where
    -- parser = fmap

    printer Empty     = ""
    printer (Value a) = C.printer a


-- |A Systemd Service record.
-- https://www.freedesktop.org/software/systemd/man/systemd.service.html
data SystemdService = SystemdService
  { unit :: Unit
  , install :: Install
  , service :: Service
  }
  deriving(Eq, Show)

instance Semigroup SystemdService where
    a <> b = SystemdService { unit    = unit a <> unit b
                            , install = install a <> install b
                            , service = service a <> service b
                            }

instance Monoid SystemdService where
    mempty =
        SystemdService { unit = mempty, install = mempty, service = mempty }

-- |A Systemd [Unit] record.
-- https://www.freedesktop.org/software/systemd/man/systemd.unit.html#%5BUnit%5D%20Section%20Options
data Unit = Unit
  { description :: EmptyDefault Description
  , documentation :: [Documentation]
  , before :: [Target]
  , after :: [Target]
  , wants :: [Target]
  , requires :: [Target]
  , conflicts :: [Target]
  }
  deriving(Eq, Show)

instance Semigroup Unit where
    a <> b = Unit { description   = description a <> description b
                  , documentation = documentation a <> documentation b
                  , before        = before a <> before b
                  , after         = after a <> after b
                  , wants         = wants a <> wants b
                  , requires      = requires a <> requires b
                  , conflicts     = conflicts a <> conflicts b
                  }

instance Monoid Unit where
    mempty = Unit { description   = mempty
                  , documentation = mempty
                  , before        = mempty
                  , after         = mempty
                  , wants         = mempty
                  , requires      = mempty
                  , conflicts     = mempty
                  }

newtype Description = Description T.Text
    deriving(Eq, Show)

instance C.Config Description where
    parser = Description <$> P.words

    printer (Description d) = d


newtype Documentation = Documentation T.Text
    deriving(Eq, Show)

instance Semigroup Documentation where
    Documentation a <> Documentation b = Documentation (a <> b)

instance Monoid Documentation where
    mempty = Documentation mempty

instance C.Config Documentation where
    parser = Documentation . C.plain <$> C.spaced (C.quoted P.word)

    printer (Documentation d) = d


newtype Target = Target T.Text
    deriving(Eq, Show)

instance Semigroup Target where
    Target a <> Target b = Target (a <> b)

instance Monoid Target where
    mempty = Target mempty

instance C.Config Target where
    parser = Target . C.plain <$> C.spaced (C.quoted P.word)

    printer (Target t) = t


-- |A Systemd [Install] record.
-- https://www.freedesktop.org/software/systemd/man/systemd.unit.html#%5BInstall%5D%20Section%20Options
data Install = Install
  { wantedBy :: [Target]
  , requiredBy :: [Target]
  }
  deriving(Eq, Show)

instance Semigroup Install where
    a <> b = Install { wantedBy   = wantedBy a <> wantedBy b
                     , requiredBy = requiredBy a <> requiredBy b
                     }

instance Monoid Install where
    mempty = Install { wantedBy = mempty, requiredBy = mempty }


-- |A Systemd [Service] record.
-- https://www.freedesktop.org/software/systemd/man/systemd.service.html#Options
-- https://www.freedesktop.org/software/systemd/man/systemd.resource-control.html#TasksMax=N
data Service = Service
  { execCondition :: [Exec]
  , execStartPre :: [Exec]
  , execStartPost :: [Exec]
  , type_ :: Type
  , execReload :: EmptyDefault Exec
  , execStop :: [Exec]
  , execStopPost :: [Exec]
  , remainAfterExit :: EmptyDefault RemainAfterExit
  , user :: EmptyDefault User
  , group :: EmptyDefault Group
  , workingDirectory :: EmptyDefault WorkingDirectory
  , standardOutput :: EmptyDefault Output
  , standardError :: EmptyDefault Output
  , tasksMax :: EmptyDefault TasksMax
  , restart :: EmptyDefault Restart
  , privateTmp :: EmptyDefault PrivateTmp
  }
  deriving(Eq, Show)

instance Semigroup Service where
    a <> b = Service
        { execCondition    = execCondition a <> execCondition b
        , execStartPre     = execStartPre a <> execStartPre b
        , execStartPost    = execStartPost a <> execStartPost b
        , type_            = type_ a <> type_ b
        , execReload       = execReload a <> execReload b
        , execStop         = execStop a <> execStop b
        , execStopPost     = execStopPost a <> execStopPost b
        , remainAfterExit  = remainAfterExit a <> remainAfterExit b
        , user             = user a <> user b
        , group            = group a <> group b
        , workingDirectory = workingDirectory a <> workingDirectory b
        , standardOutput   = standardOutput a <> standardOutput b
        , standardError    = standardError a <> standardError b
        , tasksMax         = tasksMax a <> tasksMax b
        , restart          = restart a <> restart b
        , privateTmp       = privateTmp a <> privateTmp b
        }

instance Monoid Service where
    mempty = Service { execCondition    = mempty
                     , execStartPre     = mempty
                     , execStartPost    = mempty
                     , type_            = mempty
                     , execReload       = mempty
                     , execStop         = mempty
                     , execStopPost     = mempty
                     , remainAfterExit  = mempty
                     , user             = mempty
                     , group            = mempty
                     , workingDirectory = mempty
                     , standardOutput   = mempty
                     , standardError    = mempty
                     , tasksMax         = mempty
                     , restart          = mempty
                     , privateTmp       = mempty
                     }


-- |A Systemd [Type] record.
-- https://www.freedesktop.org/software/systemd/man/systemd.service.html#Type=
data Type
  = TNothing
  | TSimple Exec
  | TExec Exec
  | TForking (EmptyDefault PIDFile) Exec
  | TOneShot [Exec]
  | TDBus BusName Exec
  | TNotify NotifyAccess Exec
  | TIdle Exec
  deriving(Eq, Show)

instance Semigroup Type where
    a           <> TNothing    = a
    TOneShot as <> TOneShot bs = TOneShot (as <> bs)
    _           <> b           = b

instance Monoid Type where
    mempty = TNothing


data InternalService = InternalService
  { iExecCondition :: [Exec]
  , iExecStartPre :: [Exec]
  , iExecStartPost :: [Exec]
  , iType :: EmptyDefault T.Text
  , iPIDFile :: EmptyDefault PIDFile
  , iRemainAfterExit :: EmptyDefault RemainAfterExit
  , iBusName :: EmptyDefault BusName
  , iNotifyAccess :: EmptyDefault NotifyAccess
  , iExecStart :: [Exec]
  , iExecReload :: EmptyDefault Exec
  , iExecStop :: [Exec]
  , iExecStopPost :: [Exec]
  , iUser :: EmptyDefault User
  , iGroup :: EmptyDefault Group
  , iWorkingDirectory :: EmptyDefault WorkingDirectory
  , iStandardOutput :: EmptyDefault Output
  , iStandardError :: EmptyDefault Output
  , iTasksMax :: EmptyDefault TasksMax
  , iRestart :: EmptyDefault Restart
  , iPrivateTmp :: EmptyDefault PrivateTmp
  }
  deriving(Eq, Show)

instance Semigroup InternalService where
    a <> b = InternalService
        { iExecCondition    = iExecCondition a <> iExecCondition b
        , iExecStartPre     = iExecStartPre a <> iExecStartPre b
        , iExecStartPost    = iExecStartPost a <> iExecStartPost b
        , iType             = iType a <> iType b
        , iPIDFile          = iPIDFile a <> iPIDFile b
        , iRemainAfterExit  = iRemainAfterExit a <> iRemainAfterExit b
        , iBusName          = iBusName a <> iBusName b
        , iNotifyAccess     = iNotifyAccess a <> iNotifyAccess b
        , iExecStart        = iExecStart a <> iExecStart b
        , iExecReload       = iExecReload a <> iExecReload b
        , iExecStop         = iExecStop a <> iExecStop b
        , iExecStopPost     = iExecStopPost a <> iExecStopPost b
        , iUser             = iUser a <> iUser b
        , iGroup            = iGroup a <> iGroup b
        , iWorkingDirectory = iWorkingDirectory a <> iWorkingDirectory b
        , iStandardOutput   = iStandardOutput a <> iStandardOutput b
        , iStandardError    = iStandardError a <> iStandardError b
        , iTasksMax         = iTasksMax a <> iTasksMax b
        , iRestart          = iRestart a <> iRestart b
        , iPrivateTmp       = iPrivateTmp a <> iPrivateTmp b
        }

instance Monoid InternalService where
    mempty = InternalService { iExecCondition    = mempty
                             , iExecStartPre     = mempty
                             , iExecStartPost    = mempty
                             , iType             = mempty
                             , iPIDFile          = mempty
                             , iRemainAfterExit  = mempty
                             , iBusName          = mempty
                             , iNotifyAccess     = mempty
                             , iExecStart        = mempty
                             , iExecReload       = mempty
                             , iExecStop         = mempty
                             , iExecStopPost     = mempty
                             , iUser             = mempty
                             , iGroup            = mempty
                             , iWorkingDirectory = mempty
                             , iStandardOutput   = mempty
                             , iStandardError    = mempty
                             , iTasksMax         = mempty
                             , iRestart          = mempty
                             , iPrivateTmp       = mempty
                             }


-- |Common record for all Exec commands like ExecStart and ExecStop.
-- https://www.freedesktop.org/software/systemd/man/systemd.service.html#ExecStart=
data Exec = Exec
  { overrideName :: Bool
  , continueOnError :: Bool
  , noEnvironmentVariableSubstitution :: Bool
  , command :: T.Text
  }
  deriving(Eq, Show)

instance Semigroup Exec where
    a <> b = Exec
        { overrideName                      = overrideName a || overrideName b
        , continueOnError = continueOnError a || continueOnError b
        , noEnvironmentVariableSubstitution =
            noEnvironmentVariableSubstitution a
                || noEnvironmentVariableSubstitution b
        , command                           = command a <> command b
        }

instance Monoid Exec where
    mempty = Exec { overrideName                      = False
                  , continueOnError                   = False
                  , noEnvironmentVariableSubstitution = False
                  , command                           = mempty
                  }


newtype User = User T.Text
  deriving(Eq, Show)

instance C.Config User where
    parser = User <$> P.word

    printer (User u) = u


newtype Group = Group T.Text
  deriving(Eq, Show)

instance C.Config Group where
    parser = Group <$> P.word

    printer (Group u) = u


newtype WorkingDirectory = WorkingDirectory T.Text
  deriving(Eq, Show)

instance C.Config WorkingDirectory where
    parser = WorkingDirectory <$> P.word

    printer (WorkingDirectory u) = u


data Output = OJournal
  deriving(Eq, Show)

instance C.Config Output where
    parser = P.choice [P.chunk "journal" $> OJournal]

    printer OJournal = "journal"


data TasksMax = TasksMax Int | TasksMaxInfinity
    deriving(Eq, Show)

instance C.Config TasksMax where
    parser =
        (TasksMax <$> P.number) <|> (P.chunk "infinity" $> TasksMaxInfinity)

    printer (TasksMax i)     = T.pack $ show i
    printer TasksMaxInfinity = "infinity"

instance Semigroup TasksMax where
    _ <> b = b

instance Monoid TasksMax where
    mempty = TasksMax 0


data Restart = RNo
             | RAlways
             | ROnSuccess
             | ROnFailure
             | ROnAbnormal
             | ROnAbort
             | ROnWatchdog
  deriving(Eq, Show)

instance C.Config Restart where
    parser = P.choice
        [ P.chunk "no" $> RNo
        , P.chunk "always" $> RAlways
        , P.chunk "on-success" $> ROnSuccess
        , P.chunk "on-failure" $> ROnFailure
        , P.chunk "on-abnormal" $> ROnAbnormal
        , P.chunk "on-abort" $> ROnAbort
        , P.chunk "on-watchdog" $> ROnWatchdog
        ]

    printer RNo         = "no"
    printer RAlways     = "always"
    printer ROnSuccess  = "on-success"
    printer ROnFailure  = "on-failure"
    printer ROnAbnormal = "on-abnormal"
    printer ROnAbort    = "on-abort"
    printer ROnWatchdog = "on-watchdog"


-- |A convenience type to represent PIDFile=.
-- https://www.freedesktop.org/software/systemd/man/systemd.service.html#PIDFile=
newtype PIDFile = PIDFile T.Text
  deriving(Eq, Show)

instance C.Config PIDFile where
    parser = PIDFile <$> P.words

    printer (PIDFile pid) = pid


-- |A convenience type to represent RemainAfterExit=.
-- https://www.freedesktop.org/software/systemd/man/systemd.service.html#RemainAfterExit=
newtype RemainAfterExit = RemainAfterExit Bool
  deriving(Eq, Show)

instance C.Config RemainAfterExit where
    parser = RemainAfterExit
        <$> P.choice [P.chunk "yes" $> True, P.chunk "no" $> False]

    printer (RemainAfterExit True ) = "yes"
    printer (RemainAfterExit False) = "no"


-- |A convenience type to represent BusName=.
-- https://www.freedesktop.org/software/systemd/man/systemd.service.html#BusName=
newtype BusName = BusName T.Text
  deriving(Eq, Show)

instance C.Config BusName where
    parser = BusName <$> P.words

    printer (BusName bus) = bus


-- |Represents a NotifyAccess=.
-- https://www.freedesktop.org/software/systemd/man/systemd.service.html#BusName=
data NotifyAccess
  = NANone
  | NAMain
  | NAExec
  | NAAll
  deriving(Eq, Show)

instance C.Config NotifyAccess where
    parser = P.choice
        [ P.chunk "none" $> NANone
        , P.chunk "main" $> NAMain
        , P.chunk "exec" $> NAExec
        , P.chunk "all" $> NAAll
        ]

    printer NANone = "none"
    printer NAMain = "main"
    printer NAExec = "exec"
    printer NAAll  = "all"


-- |A convenience type to represent PrivateTmp=.
-- https://www.freedesktop.org/software/systemd/man/systemd.exec.html#PrivateTmp=
newtype PrivateTmp = PrivateTmp Bool
  deriving(Eq, Show)

instance C.Config PrivateTmp where
    parser = PrivateTmp
        <$> P.choice [P.chunk "true" $> True, P.chunk "false" $> False]

    printer (PrivateTmp True ) = "true"
    printer (PrivateTmp False) = "false"


instance C.Config SystemdService where
    parser = P.build
        [ C.header "Unit" *> ((\u -> mempty { unit = u }) <$> C.parser)
        , C.header "Install" *> ((\i -> mempty { install = i }) <$> C.parser)
        , C.header "Service" *> ((\s -> mempty { service = s }) <$> C.parser)
        ]
    printer SystemdService {..} =
        C.printer unit <..> C.printer install <..> C.printer service


(<=>) :: C.Config c => T.Text -> c -> T.Text
a <=> b = case C.printer b of
    "" -> ""
    t  -> a <> "=" <> t


(<.>) :: (Eq t, IsString t, Semigroup t) => t -> t -> t
"" <.> b  = b
a  <.> "" = a
a  <.> b  = a <> "\n" <> b


(<..>) :: (Eq t, IsString t, Semigroup t) => t -> t -> t
"" <..> b  = b
a  <..> "" = a
a  <..> b  = a <> "\n\n" <> b


header :: (Eq t, IsString t, Semigroup t) => t -> t -> t
header _ ""   = ""
header h body = h <.> body


instance C.Config Unit where
    parser = P.build
        [ (\d -> mempty { description = pure d })
            <$> C.assignment "Description" C.parser
        , (\d -> mempty { documentation = d })
            <$> C.assignment "Documentation" (P.some C.parser)
        , (\bs -> mempty { before = bs })
            <$> C.assignment "Before" (P.some C.parser)
        , (\as -> mempty { after = as })
            <$> C.assignment "After" (P.some C.parser)
        , (\ws -> mempty { wants = ws })
            <$> C.assignment "Wants" (P.some C.parser)
        , (\rs -> mempty { requires = rs })
            <$> C.assignment "Requires" (P.some C.parser)
        , (\cs -> mempty { conflicts = cs })
            <$> C.assignment "Conflicts" (P.some C.parser)
        , P.emptyLine $> mempty
        ]

    printer Unit {..} =
        header "[Unit]"
            $   (("Description=" <>) . C.printer) description
            <.> (T.intercalate "\n" . fmap (("Documentation=" <>) . C.printer))
                    documentation
            <.> (T.intercalate "\n" . fmap (("Before=" <>) . C.printer)) before
            <.> (T.intercalate "\n" . fmap (("After=" <>) . C.printer)) after
            <.> (T.intercalate "\n" . fmap (("Wants=" <>) . C.printer)) wants
            <.> (T.intercalate "\n" . fmap (("Requires=" <>) . C.printer))
                    requires
            <.> (T.intercalate "\n" . fmap (("Conflicts=" <>) . C.printer))
                    conflicts


instance C.Config Install where
    parser = P.build
        [ (\wbs -> mempty { wantedBy = wbs })
            <$> C.assignment "WantedBy" (P.some C.parser)
        , (\rbs -> mempty { requiredBy = rbs })
            <$> C.assignment "RequiredBy" (P.some C.parser)
        , P.emptyLine $> mempty
        ]
    printer Install {..} =
        header "[Install]"
            $   (T.intercalate "\n" . fmap (("WantedBy=" <>) . C.printer))
                    wantedBy
            <.> (T.intercalate "\n" . fmap (("RequiredBy=" <>) . C.printer))
                    requiredBy


instance C.Config Service where
    parser = toService <$> parseInternalType >>= \case
        Left  e -> fail $ T.unpack e
        Right s -> return s
      where
        toService :: InternalService -> Either T.Text Service
        toService InternalService {..} =
            let
                t = case iType of
                    Empty          -> TSimple <$> last' "simple" iExecStart
                    Value "simple" -> TSimple <$> last' "simple" iExecStart
                    Value "exec"   -> TExec <$> last' "exec" iExecStart
                    Value "forking" ->
                        TForking
                            <$> Right iPIDFile
                            <*> last' "forking" iExecStart
                    Value "oneshot" -> TOneShot <$> Right iExecStart
                    Value "dbus" ->
                        TDBus
                            <$> asEither "iBusName" iBusName
                            <*> last' "dbus" iExecStart
                    Value "notify" ->
                        TNotify
                            <$> asEither "iNotifyAccess" iNotifyAccess
                            <*> last' "notify" iExecStart
                    Value "idle" -> TIdle <$> last' "idle" iExecStart
                    Value other ->
                        Left $ "Type with unknown value '" <> other <> "'"
                s = mempty { execCondition    = iExecCondition
                           , execStartPre     = iExecStartPre
                           , execStartPost    = iExecStartPost
                           , execReload       = iExecReload
                           , execStop         = iExecStop
                           , execStopPost     = iExecStopPost
                           , remainAfterExit  = iRemainAfterExit
                           , user             = iUser
                           , group            = iGroup
                           , workingDirectory = iWorkingDirectory
                           , standardOutput   = iStandardOutput
                           , standardError    = iStandardError
                           , tasksMax         = iTasksMax
                           , restart          = iRestart
                           , privateTmp       = iPrivateTmp
                           }
            in
                case t of
                    Left  e  -> Left e
                    Right t' -> Right s { type_ = t' }

        last' :: (IsString e, Semigroup e) => e -> [a] -> Either e a
        last' e [] =
            Left $ "Expected 'ExecStart' assignment for Type='" <> e <> "'"
        last' _ [x     ] = Right x
        last' e (_ : xs) = last' e xs

        asEither :: e -> EmptyDefault a -> Either e a
        asEither e Empty     = Left e
        asEither _ (Value a) = Right a

        parseInternalType :: P.Parser InternalService
        parseInternalType = P.build
            [ (\e -> mempty { iExecCondition = pure e })
                <$> C.assignment "ExecCondition" C.parser
            , (\e -> mempty { iExecStartPre = pure e })
                <$> C.assignment "ExecStartPre" C.parser
            , (\e -> mempty { iExecStartPost = pure e })
                <$> C.assignment "ExecStartPost" C.parser
            , (\t -> mempty { iType = pure t }) <$> C.assignment "Type" P.word
            , (\p -> mempty { iPIDFile = pure p })
                <$> C.assignment "PIDFile" C.parser
            , (\r -> mempty { iRemainAfterExit = pure r })
                <$> C.assignment "RemainAfterExit" C.parser
            , (\b -> mempty { iBusName = pure b })
                <$> C.assignment "BusName" C.parser
            , (\n -> mempty { iNotifyAccess = pure n })
                <$> C.assignment "NotifyAccess" C.parser
            , (\e -> mempty { iExecStart = pure e })
                <$> C.assignment "ExecStart" C.parser
            , (\e -> mempty { iExecReload = pure e })
                <$> C.assignment "ExecReload" C.parser
            , (\e -> mempty { iExecStop = pure e })
                <$> C.assignment "ExecStop" C.parser
            , (\e -> mempty { iExecStopPost = pure e })
                <$> C.assignment "ExecStopPost" C.parser
            , (\u -> mempty { iUser = pure u }) <$> C.assignment "User" C.parser
            , (\g -> mempty { iGroup = pure g })
                <$> C.assignment "Group" C.parser
            , (\w -> mempty { iWorkingDirectory = pure w })
                <$> C.assignment "WorkingDirectory" C.parser
            , (\o -> mempty { iStandardOutput = pure o })
                <$> C.assignment "StandardOutput" C.parser
            , (\e -> mempty { iStandardError = pure e })
                <$> C.assignment "StandardError" C.parser
            , (\t -> mempty { iTasksMax = pure t })
                <$> C.assignment "TasksMax" C.parser
            , (\r -> mempty { iRestart = pure r })
                <$> C.assignment "Restart" C.parser
            , (\p -> mempty { iPrivateTmp = pure $ p })
                <$> C.assignment "PrivateTmp" C.parser
            , P.emptyLine $> mempty
            ]

    printer Service {..} =
        header "[Service]"
            $   (\case
                    TNothing     -> ""
                    TSimple exec -> "Type=simple" <.> "ExecStart" <=> exec
                    TExec   exec -> "Type=exec" <.> "ExecStart" <=> exec
                    TForking pidFile exec ->
                        "Type=forking"
                            <.> ("PIDFile" <=> pidFile)
                            <.> ("ExecStart" <=> exec)
                    TOneShot execs ->
                        "Type=oneshot"
                            <.> (T.intercalate "\n" . fmap ("ExecStart" <=>))
                                    execs
                    TDBus busName exec ->
                        "Type=dbus"
                            <.> "BusName"
                            <=> busName
                            <.> "ExecStart"
                            <=> exec
                    TNotify notifyAccess exec ->
                        "Type=notify"
                            <.> "NotifyAccess"
                            <=> notifyAccess
                            <.> "ExecStart"
                            <=> exec
                    TIdle exec -> "Type=idle" <.> "ExecStart" <=> exec
                )
                    type_
            <.> ("User" <=> user)
            <.> ("Group" <=> group)
            <.> ("WorkingDirectory" <=> workingDirectory)
            <.> ("StandardOutput" <=> standardOutput)
            <.> ("StandardError" <=> standardError)
            <.> ("TasksMax" <=> tasksMax)
            <.> ("Restart" <=> restart)
            <.> ("PrivateTmp" <=> privateTmp)
            <.> ("RemainAfterExit" <=> remainAfterExit)
            <.> (T.intercalate "\n" . fmap ("ExecCondition=" <=>)) execCondition
            <.> (T.intercalate "\n" . fmap ("ExecStartPre=" <=>)) execStartPre
            <.> (T.intercalate "\n" . fmap ("ExecStartPost=" <=>)) execStartPost
            <.> ("ExecReload=" <=> execReload)
            <.> (T.intercalate "\n" . fmap ("ExecStop=" <=>)) execStop
            <.> (T.intercalate "\n" . fmap ("ExecStopPost=" <=>)) execStopPost


instance C.Config Exec where
    parser =
        P.build
                [ P.char '@' $> (mempty { overrideName = True })
                , P.char '-' $> (mempty { continueOnError = True })
                , P.char ':'
                    $> (mempty { noEnvironmentVariableSubstitution = True })
                ]
            <> ((\l -> mempty { command = l }) <$> P.line)
    printer Exec {..} =
        (if overrideName then "@" else "")
            <> (if continueOnError then "-" else "")
            <> (if noEnvironmentVariableSubstitution then ":" else "")
            <> command

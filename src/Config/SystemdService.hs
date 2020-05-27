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
    , genTargets
    )
where


import           Control.Applicative            ( (<|>) )
import           Data.Functor                   ( ($>) )
import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as T
import           Data.String                    ( IsString )
import qualified Parser                        as P
import           Config                         ( (</>) )
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
    parser = pure <$> C.parser

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

instance Semigroup Description where
    Description a <> Description b = Description (a <> b)

instance Monoid Description where
    mempty = Description ""

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

parseTarget = P.some (Target . C.plain <$> C.spaced (C.quoted P.word))

genTargets p (C.FieldsTree (Just values) m) | Map.null m = mconcat $ map
    (C.generateEither . C.mapLeft (C.ParseError p) . C.parse parseTarget)
    values
genTargets p _ = C.generateError $ C.UnknownPath p

--instance Semigroup Target where
--    Target a <> Target b = Target (a <> b)

--instance Monoid Target where
--    mempty = Target mempty

--instance C.Config Target where
--    parser = Target . C.plain <$> C.spaced (C.quoted P.word)
--
--    printer (Target t) = t


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

instance Semigroup User where
    _ <> b = b

instance C.Config User where
    parser = User <$> P.word

    printer (User u) = u


newtype Group = Group T.Text
  deriving(Eq, Show)

instance Semigroup Group where
    _ <> b = b

instance C.Config Group where
    parser = Group <$> P.word

    printer (Group u) = u


newtype WorkingDirectory = WorkingDirectory T.Text
  deriving(Eq, Show)

instance Semigroup WorkingDirectory where
    _ <> b = b

instance C.Config WorkingDirectory where
    parser = WorkingDirectory <$> P.word

    printer (WorkingDirectory u) = u


data Output = OJournal
  deriving(Eq, Show)

instance Semigroup Output where
    _ <> b = b

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

instance Semigroup Restart where
    _ <> b = b

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

instance Semigroup PIDFile where
    _ <> b = b

instance C.Config PIDFile where
    parser = PIDFile <$> P.words

    printer (PIDFile pid) = pid


-- |A convenience type to represent RemainAfterExit=.
-- https://www.freedesktop.org/software/systemd/man/systemd.service.html#RemainAfterExit=
newtype RemainAfterExit = RemainAfterExit Bool
  deriving(Eq, Show)

instance Semigroup RemainAfterExit where
    _ <> b = b

instance C.Config RemainAfterExit where
    parser = RemainAfterExit
        <$> P.choice [P.chunk "yes" $> True, P.chunk "no" $> False]

    printer (RemainAfterExit True ) = "yes"
    printer (RemainAfterExit False) = "no"


-- |A convenience type to represent BusName=.
-- https://www.freedesktop.org/software/systemd/man/systemd.service.html#BusName=
newtype BusName = BusName T.Text
  deriving(Eq, Show)

instance Semigroup BusName where
    _ <> b = b

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

instance Semigroup NotifyAccess where
    _ <> b = b

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

instance Semigroup PrivateTmp where
    _ <> b = b

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

    gen path (C.FieldsTree Nothing m) = flip Map.foldMapWithKey m $ \k v ->
        case k of
            "Unit" -> mconcat $ map
                (fmap (\v' -> mempty { unit = v' }) . C.gen (path </> "Unit"))
                v
            "Install" -> mconcat $ map
                ( fmap (\v' -> mempty { install = v' })
                . C.gen (path </> "Install")
                )
                v
            "Service" -> mconcat $ map
                ( fmap (\v' -> mempty { service = v' })
                . C.gen (path </> "Service")
                )
                v
            other -> C.generateError $ C.UnknownPath (path </> other)
    gen path _ = C.generateError $ C.UnknownPath path

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
        [ (\d -> mempty { description = d })
            <$> C.assignment "Description" C.parser
        , (\d -> mempty { documentation = d })
            <$> C.assignment "Documentation" (P.some C.parser)
        , (\bs -> mempty { before = bs }) <$> C.assignment "Before" parseTarget
        , (\as -> mempty { after = as }) <$> C.assignment "After" parseTarget
        , (\ws -> mempty { wants = ws }) <$> C.assignment "Wants" parseTarget
        , (\rs -> mempty { requires = rs })
            <$> C.assignment "Requires" parseTarget
        , (\cs -> mempty { conflicts = cs })
            <$> C.assignment "Conflicts" parseTarget
        , P.emptyLine $> mempty
        ]

    printer Unit {..} =
        header "[Unit]"
            $   (("Description=" <>) . C.printer) description
            <.> (T.intercalate "\n" . fmap (("Documentation=" <>) . C.printer))
                    documentation
            <.> (T.intercalate "\n" . fmap (("Before=" <>) . (\(Target t) -> t))
                )
                    before
            <.> (T.intercalate "\n" . fmap (("After=" <>) . (\(Target t) -> t)))
                    after
            <.> (T.intercalate "\n" . fmap (("Wants=" <>) . (\(Target t) -> t)))
                    wants
            <.> ( T.intercalate "\n"
                . fmap (("Requires=" <>) . (\(Target t) -> t))
                )
                    requires
            <.> ( T.intercalate "\n"
                . fmap (("Conflicts=" <>) . (\(Target t) -> t))
                )
                    conflicts

    gen path (C.FieldsTree Nothing m) = flip Map.foldMapWithKey m $ \k v ->
        case k of
            "Description" -> mconcat $ map
                ( fmap (\v' -> mempty { description = v' })
                . C.gen (path </> "Description")
                )
                v
            "Documentation" -> mconcat $ map
                ( fmap (\v' -> mempty { documentation = pure v' })
                . C.gen (path </> "Documentation")
                )
                v
            "Before" -> mconcat $ map
                ( fmap (\v' -> mempty { before = v' })
                . genTargets (path </> "Before")
                )
                v
            "After" -> mconcat $ map
                ( fmap (\v' -> mempty { after = v' })
                . genTargets (path </> "After")
                )
                v
            "Wants" -> mconcat $ map
                ( fmap (\v' -> mempty { wants = v' })
                . genTargets (path </> "Wants")
                )
                v
            "Requires" -> mconcat $ map
                ( fmap (\v' -> mempty { requires = v' })
                . genTargets (path </> "Requires")
                )
                v
            "Conflicts" -> mconcat $ map
                ( fmap (\v' -> mempty { conflicts = v' })
                . genTargets (path </> "Conflicts")
                )
                v
            other -> C.generateError $ C.UnknownPath (path </> other)
    gen path _ = C.generateError $ C.UnknownPath path


instance C.Config Install where
    parser = P.build
        [ (\wbs -> mempty { wantedBy = wbs })
            <$> C.assignment "WantedBy" parseTarget
        , (\rbs -> mempty { requiredBy = rbs })
            <$> C.assignment "RequiredBy" parseTarget
        , P.emptyLine $> mempty
        ]
    printer Install {..} =
        header "[Install]"
            $ (T.intercalate "\n" . fmap (("WantedBy=" <>) . (\(Target t) -> t))
              )
                  wantedBy
            <.> ( T.intercalate "\n"
                . fmap (("RequiredBy=" <>) . (\(Target t) -> t))
                )
                    requiredBy

    gen path (C.FieldsTree Nothing m) = flip Map.foldMapWithKey m $ \k v ->
        case k of
            "WantedBy" -> mconcat $ map
                ( fmap (\v' -> mempty { wantedBy = v' })
                . genTargets (path </> "WantedBy")
                )
                v
            "RequiredBy" -> mconcat $ map
                ( fmap (\v' -> mempty { requiredBy = v' })
                . genTargets (path </> "RequiredBy")
                )
                v
            other -> C.generateError $ C.UnknownPath (path </> other)
    gen path _ = C.generateError $ C.UnknownPath path


instance C.Config Service where
    parser = toService <$> parseInternalType >>= \case
        Left  e -> fail $ T.unpack e
        Right s -> return s
      where
        parseInternalType :: P.Parser InternalService
        parseInternalType = P.build
            [ (\e -> mempty { iExecCondition = pure e })
                <$> C.assignment "ExecCondition" C.parser
            , (\e -> mempty { iExecStartPre = pure e })
                <$> C.assignment "ExecStartPre" C.parser
            , (\e -> mempty { iExecStartPost = pure e })
                <$> C.assignment "ExecStartPost" C.parser
            , (\t -> mempty { iType = pure t }) <$> C.assignment "Type" P.word
            , (\p -> mempty { iPIDFile = p })
                <$> C.assignment "PIDFile" C.parser
            , (\r -> mempty { iRemainAfterExit = r })
                <$> C.assignment "RemainAfterExit" C.parser
            , (\b -> mempty { iBusName = b })
                <$> C.assignment "BusName" C.parser
            , (\n -> mempty { iNotifyAccess = n })
                <$> C.assignment "NotifyAccess" C.parser
            , (\e -> mempty { iExecStart = pure e })
                <$> C.assignment "ExecStart" C.parser
            , (\e -> mempty { iExecReload = e })
                <$> C.assignment "ExecReload" C.parser
            , (\e -> mempty { iExecStop = pure e })
                <$> C.assignment "ExecStop" C.parser
            , (\e -> mempty { iExecStopPost = pure e })
                <$> C.assignment "ExecStopPost" C.parser
            , (\u -> mempty { iUser = u }) <$> C.assignment "User" C.parser
            , (\g -> mempty { iGroup = g }) <$> C.assignment "Group" C.parser
            , (\w -> mempty { iWorkingDirectory = w })
                <$> C.assignment "WorkingDirectory" C.parser
            , (\o -> mempty { iStandardOutput = o })
                <$> C.assignment "StandardOutput" C.parser
            , (\e -> mempty { iStandardError = e })
                <$> C.assignment "StandardError" C.parser
            , (\t -> mempty { iTasksMax = t })
                <$> C.assignment "TasksMax" C.parser
            , (\r -> mempty { iRestart = r })
                <$> C.assignment "Restart" C.parser
            , (\p -> mempty { iPrivateTmp = p })
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

    -- gen path (C.FieldsTree Nothing m) = flip Map.foldMapWithKey m $ \k v ->
    --     case k of
    --         "WantedBy" -> mconcat $ map
    --             ( fmap (\v' -> mempty { wantedBy = v' })
    --             . genTargets (path </> "WantedBy")
    --             )
    --             v
    --         "RequiredBy" -> mconcat $ map
    --             ( fmap (\v' -> mempty { requiredBy = v' })
    --             . genTargets (path </> "RequiredBy")
    --             )
    --             v
    --         other -> C.generateError $ C.UnknownPath (path </> other)
    -- gen path _ = C.generateError $ C.UnknownPath path

    -- mconcat $ map
    --     (C.generateEither . C.mapLeft (C.ParseError p) . C.parse parseTarget)
    --     values

    gen path (C.FieldsTree Nothing m) =
        parseInternal
            >>= C.generateEither
            .   C.mapLeft (C.ParseError path)
            .   toService
      where
        genExecs p (C.FieldsTree (Just values) m') | Map.null m' = mconcat $ map
            (C.generateEither . C.mapLeft (C.ParseError p) . C.parse
                (P.some C.parser)
            )
            values
        genExecs p _ = C.generateError $ C.UnknownPath p

        genUser p (C.FieldsTree (Just values) m') | Map.null m' = mconcat $ map
            (C.generateEither . C.mapLeft (C.ParseError p) . C.parse C.parser)
            values
        genUser p _ = C.generateError $ C.UnknownPath p

        parseInternal = flip Map.foldMapWithKey m $ \k v -> case k of
            "ExecCondition" -> mconcat $ map
                ( fmap (\v' -> mempty { iExecCondition = v' })
                . genExecs (path </> "ExecCondition")
                )
                v
            --"ExecStartPre" ->
            --    mapLeft (C.ParseError $ C.Path path)
            --        $ mapRight (\v -> mempty { iExecStartPre = v })
            --        $ C.parse (P.some C.parser) value
            --"ExecStartPost" ->
            --    mapLeft (C.ParseError $ C.Path path)
            --        $ mapRight (\v -> mempty { iExecStartPost = v })
            --        $ C.parse (P.some C.parser) value
            --"Type" ->
            --    mapLeft (C.ParseError $ C.Path path)
            --        $ mapRight (\v -> mempty { iType = pure v })
            --        $ C.parse P.word value
            "ExecStart" -> mconcat $ map
                ( fmap (\v' -> mempty { iExecStart = v' })
                . genExecs (path </> "ExecStart")
                )
                v
            --"PIDFile" ->
            --    mapLeft (C.ParseError $ C.Path path)
            --        $ mapRight (\v -> mempty { iPIDFile = v })
            --        $ C.parse C.parser value
            --"RemainAfterExit" ->
            --    mapLeft (C.ParseError $ C.Path path)
            --        $ mapRight (\v -> mempty { iRemainAfterExit = v })
            --        $ C.parse C.parser value
            --"BusName" ->
            --    mapLeft (C.ParseError $ C.Path path)
            --        $ mapRight (\v -> mempty { iBusName = v })
            --        $ C.parse C.parser value
            --"NotifyAccess" ->
            --    mapLeft (C.ParseError $ C.Path path)
            --        $ mapRight (\v -> mempty { iNotifyAccess = v })
            --        $ C.parse C.parser value
            --"ExecReload" ->
            --    mapLeft (C.ParseError $ C.Path path)
            --        $ mapRight (\v -> mempty { iExecReload = v })
            --        $ C.parse C.parser value
            --"ExecStop" ->
            --    mapLeft (C.ParseError $ C.Path path)
            --        $ mapRight (\v -> mempty { iExecStop = v })
            --        $ C.parse (P.some C.parser) value
            --"ExecStopPost" ->
            --    mapLeft (C.ParseError $ C.Path path)
            --        $ mapRight (\v -> mempty { iExecStopPost = v })
            --        $ C.parse (P.some C.parser) value
            "User" -> mconcat $ map
                (fmap (\v' -> mempty { iUser = v' }) . genUser (path </> "User")
                )
                v
            --"Group" ->
            --    mapLeft (C.ParseError $ C.Path path)
            --        $ mapRight (\v -> mempty { iGroup = v })
            --        $ C.parse C.parser value
            --"WorkingDirectory" ->
            --    mapLeft (C.ParseError $ C.Path path)
            --        $ mapRight (\v -> mempty { iWorkingDirectory = v })
            --        $ C.parse C.parser value
            --"StandardOutput" ->
            --    mapLeft (C.ParseError $ C.Path path)
            --        $ mapRight (\v -> mempty { iStandardOutput = v })
            --        $ C.parse C.parser value
            --"StandardError" ->
            --    mapLeft (C.ParseError $ C.Path path)
            --        $ mapRight (\v -> mempty { iStandardError = v })
            --        $ C.parse C.parser value
            --"TasksMax" ->
            --    mapLeft (C.ParseError $ C.Path path)
            --        $ mapRight (\v -> mempty { iTasksMax = v })
            --        $ C.parse C.parser value
            --"Restart" ->
            --    mapLeft (C.ParseError $ C.Path path)
            --        $ mapRight (\v -> mempty { iRestart = v })
            --        $ C.parse C.parser value
            --"PrivateTmp" ->
            --    mapLeft (C.ParseError $ C.Path path)
            --        $ mapRight (\v -> mempty { iPrivateTmp = v })
            --        $ C.parse C.parser value
            other -> C.generateError $ C.UnknownPath (path </> other)
    gen path _ = C.generateError $ C.UnknownPath path

typeFromInternalService :: InternalService -> Either T.Text Type
typeFromInternalService InternalService {..} = case iType of
    Empty          -> TSimple <$> last' "simple" iExecStart
    Value "simple" -> TSimple <$> last' "simple" iExecStart
    Value "exec"   -> TExec <$> last' "exec" iExecStart
    Value "forking" ->
        TForking <$> Right iPIDFile <*> last' "forking" iExecStart
    Value "oneshot" -> TOneShot <$> Right iExecStart
    Value "dbus" ->
        TDBus <$> asEither "iBusName" iBusName <*> last' "dbus" iExecStart
    Value "notify" ->
        TNotify
            <$> asEither "iNotifyAccess" iNotifyAccess
            <*> last' "notify" iExecStart
    Value "idle" -> TIdle <$> last' "idle" iExecStart
    Value other  -> Left $ "Type with unknown value '" <> other <> "'"
  where
    last' :: (IsString e, Semigroup e) => e -> [a] -> Either e a
    last' e [] =
        Left $ "Expected 'ExecStart' assignment for Type='" <> e <> "'"
    last' _ [x     ] = Right x
    last' e (_ : xs) = last' e xs

    asEither :: e -> EmptyDefault a -> Either e a
    asEither e Empty     = Left e
    asEither _ (Value a) = Right a


toService :: InternalService -> Either T.Text Service
toService i@InternalService {..} =
    let s = mempty { execCondition    = iExecCondition
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
    in  case typeFromInternalService i of
            Left  e  -> Left e
            Right t' -> Right s { type_ = t' }


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

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
    ( parse
    , parseUnit
    , parseInstall
    , parseService
    , parseExec
    , EmptyDefault(..)
    , SystemdService(..)
    , Unit(..)
    , Install(..)
    , Service(..)
    , Type(..)
    , Exec(..)
    , PIDFile(..)
    , RemainAfterExit(..)
    , BusName(..)
    , NotifyAccess(..)
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
  { description :: T.Text
  , documentation :: [T.Text]
  , before :: [T.Text]
  , after :: [T.Text]
  , wants :: [T.Text]
  , requires :: [T.Text]
  , conflicts :: [T.Text]
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


-- |A Systemd [Install] record.
-- https://www.freedesktop.org/software/systemd/man/systemd.unit.html#%5BInstall%5D%20Section%20Options
data Install = Install
  { wantedBy :: [T.Text]
  , requiredBy :: [T.Text]
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
  , user :: EmptyDefault T.Text
  , group :: EmptyDefault T.Text
  , workingDirectory :: EmptyDefault T.Text
  , standardOutput :: Output
  , standardError :: Output
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
  | TForking (Maybe PIDFile) Exec
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
  , iUser :: EmptyDefault T.Text
  , iGroup :: EmptyDefault T.Text
  , iWorkingDirectory :: EmptyDefault T.Text
  , iStandardOutput :: Output
  , iStandardError :: Output
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


data Output = ONothing
            | OJournal
  deriving(Eq, Show)

instance Semigroup Output where
    a <> ONothing = a
    _ <> b        = b

instance Monoid Output where
    mempty = ONothing


data TasksMax = TasksMax Int | TasksMaxInfinity
    deriving(Eq, Show)

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


-- |A convenience type to represent PIDFile=.
-- https://www.freedesktop.org/software/systemd/man/systemd.service.html#PIDFile=
newtype PIDFile = PIDFile T.Text
  deriving(Eq, Show)

-- |A convenience type to represent RemainAfterExit=.
-- https://www.freedesktop.org/software/systemd/man/systemd.service.html#RemainAfterExit=
newtype RemainAfterExit = RemainAfterExit Bool
  deriving(Eq, Show)

-- |A convenience type to represent BusName=.
-- https://www.freedesktop.org/software/systemd/man/systemd.service.html#BusName=
newtype BusName = BusName T.Text
  deriving(Eq, Show)

-- |Represents a NotifyAccess=.
-- https://www.freedesktop.org/software/systemd/man/systemd.service.html#BusName=
data NotifyAccess
  = NANone
  | NAMain
  | NAExec
  | NAAll
  deriving(Eq, Show)

-- |A convenience type to represent PrivateTmp=.
-- https://www.freedesktop.org/software/systemd/man/systemd.exec.html#PrivateTmp=
newtype PrivateTmp = PrivateTmp Bool
  deriving(Eq, Show)


-- |Parse a Systemd Service.
parse :: P.Parser SystemdService
parse = P.build
    [ C.header "Unit" *> ((\u -> mempty { unit = u }) <$> parseUnit)
    , C.header "Install" *> ((\i -> mempty { install = i }) <$> parseInstall)
    , C.header "Service" *> ((\s -> mempty { service = s }) <$> parseService)
    ]

-- |Parse a Systemd Unit section.
parseUnit :: P.Parser Unit
parseUnit = P.build
    [ (\d -> mempty { description = d }) <$> C.assignment "Description" P.words
    , (\d -> mempty { documentation = C.plain <$> d }) <$> C.assignment
        "Documentation"
        (P.some $ C.spaced $ C.quoted P.word)
    , (\bs -> mempty { before = C.plain <$> bs })
        <$> C.assignment "Before" (P.some $ C.spaced $ C.quoted P.word)
    , (\as -> mempty { after = C.plain <$> as })
        <$> C.assignment "After" (P.some $ C.spaced $ C.quoted P.word)
    , (\ws -> mempty { wants = C.plain <$> ws })
        <$> C.assignment "Wants" (P.some $ C.spaced $ C.quoted P.word)
    , (\rs -> mempty { requires = C.plain <$> rs })
        <$> C.assignment "Requires" (P.some $ C.spaced $ C.quoted P.word)
    , (\rs -> mempty { conflicts = C.plain <$> rs })
        <$> C.assignment "Conflicts" (P.some $ C.spaced $ C.quoted P.word)
    , P.emptyLine $> mempty
    ]

-- |Parse a Systemd Install section.
parseInstall :: P.Parser Install
parseInstall = P.build
    [ (\wbs -> mempty { wantedBy = C.plain <$> wbs })
        <$> C.assignment "WantedBy" (P.some $ C.spaced $ C.quoted P.word)
    , (\rbs -> mempty { requiredBy = C.plain <$> rbs })
        <$> C.assignment "RequiredBy" (P.some $ C.spaced $ C.quoted P.word)
    , P.emptyLine $> mempty
    ]

-- |Parse a Systemd Service section.
parseService :: P.Parser Service
parseService = toService <$> parseInternalType >>= \case
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
                        <$> Right (asMaybe iPIDFile)
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

    asMaybe :: EmptyDefault a -> Maybe a
    asMaybe Empty     = Nothing
    asMaybe (Value v) = Just v

    asEither :: e -> EmptyDefault a -> Either e a
    asEither e Empty     = Left e
    asEither _ (Value a) = Right a

    parseInternalType :: P.Parser InternalService
    parseInternalType = P.build
        [ (\e -> mempty { iExecCondition = pure e })
            <$> C.assignment "ExecCondition" parseExec
        , (\e -> mempty { iExecStartPre = pure e })
            <$> C.assignment "ExecStartPre" parseExec
        , (\e -> mempty { iExecStartPost = pure e })
            <$> C.assignment "ExecStartPost" parseExec
        , (\t -> mempty { iType = pure t }) <$> C.assignment "Type" P.word
        , (\p -> mempty { iPIDFile = pure $ PIDFile p })
            <$> C.assignment "PIDFile" P.words
        , (\r -> mempty { iRemainAfterExit = pure $ RemainAfterExit r })
            <$> C.assignment
                    "RemainAfterExit"
                    (P.choice [P.chunk "yes" $> True, P.chunk "no" $> False])
        , (\b -> mempty { iBusName = pure $ BusName b })
            <$> C.assignment "BusName" P.words
        , (\n -> mempty { iNotifyAccess = pure n }) <$> C.assignment
            "NotifyAccess"
            (P.choice
                [ P.chunk "none" $> NANone
                , P.chunk "main" $> NAMain
                , P.chunk "exec" $> NAExec
                , P.chunk "all" $> NAAll
                ]
            )
        , (\e -> mempty { iExecStart = pure e })
            <$> C.assignment "ExecStart" parseExec
        , (\e -> mempty { iExecReload = pure e })
            <$> C.assignment "ExecReload" parseExec
        , (\e -> mempty { iExecStop = pure e })
            <$> C.assignment "ExecStop" parseExec
        , (\e -> mempty { iExecStopPost = pure e })
            <$> C.assignment "ExecStopPost" parseExec
        , (\u -> mempty { iUser = pure u }) <$> C.assignment "User" P.words
        , (\g -> mempty { iGroup = pure g }) <$> C.assignment "Group" P.words
        , (\w -> mempty { iWorkingDirectory = pure w })
            <$> C.assignment "WorkingDirectory" P.words
        , (\o -> mempty { iStandardOutput = o }) <$> C.assignment
            "StandardOutput"
            (P.choice [P.chunk "journal" $> OJournal])
        , (\e -> mempty { iStandardError = e }) <$> C.assignment
            "StandardError"
            (P.choice [P.chunk "journal" $> OJournal])
        , (\t -> mempty { iTasksMax = t }) <$> C.assignment
            "TasksMax"
            (   (Value . TasksMax <$> P.number)
            <|> (P.chunk "infinity" $> Value TasksMaxInfinity)
            )
        , (\r -> mempty { iRestart = pure r }) <$> C.assignment
            "Restart"
            (P.choice
                [ P.chunk "no" $> RNo
                , P.chunk "always" $> RAlways
                , P.chunk "on-success" $> ROnSuccess
                , P.chunk "on-failure" $> ROnFailure
                , P.chunk "on-abnormal" $> ROnAbnormal
                , P.chunk "on-abort" $> ROnAbort
                , P.chunk "on-watchdog" $> ROnWatchdog
                ]
            )
        , (\p -> mempty { iPrivateTmp = pure $ PrivateTmp p }) <$> C.assignment
            "PrivateTmp"
            (P.choice [P.chunk "true" $> True, P.chunk "false" $> False])
        , P.emptyLine $> mempty
        ]


-- |Parse a Systemd Exec command.
parseExec :: P.Parser Exec
parseExec =
    P.build
            [ P.char '@' $> (mempty { overrideName = True })
            , P.char '-' $> (mempty { continueOnError = True })
            , P.char ':'
                $> (mempty { noEnvironmentVariableSubstitution = True })
            ]
        <> ((\l -> mempty { command = l }) <$> P.line)

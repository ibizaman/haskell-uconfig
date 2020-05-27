{-# LANGUAGE QuasiQuotes #-}

module Config.SystemdServiceSpec
    ( spec
    )
where


import qualified Test.Hspec                    as H
import qualified Test.Hspec.Expectations.Pretty
                                               as HPP

import           Text.RawString.QQ

import           Config.SystemdService
import qualified Config                        as C
import qualified Parser                        as P


spec :: H.Spec
spec = do
    H.describe "parse exec" $ do
        H.it "parses empty command" $ C.parse C.parser "" `HPP.shouldBe` Right
            (mempty :: Exec)
        H.it "parses simple command"
            $              C.parse C.parser "xyz"
            `HPP.shouldBe` Right ((mempty :: Exec) { command = "xyz" })
        H.it "parses simple command with spaces"
            $              C.parse C.parser "x y z"
            `HPP.shouldBe` Right ((mempty :: Exec) { command = "x y z" })
        H.it "parses command with options"
            $              C.parse C.parser "@-xy@-z"
            `HPP.shouldBe` Right
                               ((mempty :: Exec) { command         = "xy@-z"
                                                 , overrideName    = True
                                                 , continueOnError = True
                                                 }
                               )
    H.describe "parseService" $ do
        H.it "fails if no Type"
            $              C.parse (C.parser :: P.Parser Service) ""
            `HPP.shouldBe` Left
                               "input:1:1:\n  |\n1 | <empty line>\n  | ^\nExpected 'ExecStart' assignment for Type='simple'\n"
        H.it "fails if wrong Type"
            $              C.parse (C.parser :: P.Parser Service) "Type=other"
            `HPP.shouldBe` Left
                               "input:1:11:\n  |\n1 | Type=other\n  |           ^\nType with unknown value 'other'\n"
        H.it "fails if Type is right but no ExecStart"
            $              C.parse (C.parser :: P.Parser Service) "Type=simple"
            `HPP.shouldBe` Left
                               "input:1:12:\n  |\n1 | Type=simple\n  |            ^\nExpected 'ExecStart' assignment for Type='simple'\n"
        H.it "succeeds with Type and ExecStart"
            $              C.parse C.parser "ExecStart=mycmd\nType=simple\n"
            `HPP.shouldBe` Right
                               ((mempty :: Service)
                                   { type_ = TSimple
                                                 $ mempty { command = "mycmd" }
                                   }
                               )
        H.it "succeeds with Type and ExecStart"
            $ C.parse C.parser "Type=simple\n\nExecStart=my cmd\n[other]"
            `HPP.shouldBe` Right
                               ((mempty :: Service)
                                   { type_ = TSimple
                                                 $ mempty { command = "my cmd" }
                                   }
                               )
        H.it "succeeds with Type and ExecStart with spaces"
            $ C.parse C.parser "Type=simple\n\nExecStart=m y cm d\n[other]"
            `HPP.shouldBe` Right
                               ((mempty :: Service)
                                   { type_ = TSimple $ mempty
                                                 { command = "m y cm d"
                                                 }
                                   }
                               )
        H.it "succeeds with ExecStart before Type"
            $ C.parse C.parser "\nExecStart=mycmd\n\nType=simple\n[other]"
            `HPP.shouldBe` Right
                               ((mempty :: Service)
                                   { type_ = TSimple
                                                 $ mempty { command = "mycmd" }
                                   }
                               )
        H.it "succeeds with ExecStart with spaces before Type"
            $ C.parse C.parser "\nExecStart=m y cm d\n\nType=simple\n[other]"
            `HPP.shouldBe` Right
                               ((mempty :: Service)
                                   { type_ = TSimple $ mempty
                                                 { command = "m y cm d"
                                                 }
                                   }
                               )
    H.describe "parseUnit" $ do
        H.it "with before"
            $ C.parse C.parser "Before=one two\nBefore=three\n[Service]\n\n"
            `HPP.shouldBe` Right
                               ((mempty :: Unit)
                                   { before = [ Target "one"
                                              , Target "two"
                                              , Target "three"
                                              ]
                                   }
                               )
        H.it "with description"
            $ C.parse C.parser "\n\nDescription=my service\n[Service]\n\n"
            `HPP.shouldBe` Right
                               ((mempty :: Unit)
                                   { description = Value
                                       $ Description "my service"
                                   }
                               )
    H.describe "parseInstall" $ do
        H.it "empty" $ C.parse C.parser "\n[Service]\n\n" `HPP.shouldBe` Right
            (mempty :: Install)
        H.it "wantedBy"
            $ C.parse C.parser "\nWantedBy=default.target\n\n[Service]\n\n"
            `HPP.shouldBe` Right
                               ((mempty :: Install)
                                   { wantedBy = [Target "default.target"]
                                   }
                               )
        H.it "wantedBy and requiredBy"
            $              C.parse
                               C.parser
                               "\nWantedBy=default.target\n\nRequiredBy=other.target\n\nRequiredBy=4\nWantedBy=1 2 3\n[Service]\n\n"
            `HPP.shouldBe` Right
                               ((mempty :: Install)
                                   { wantedBy   = [ Target "default.target"
                                                  , Target "1"
                                                  , Target "2"
                                                  , Target "3"
                                                  ]
                                   , requiredBy = [ Target "other.target"
                                                  , Target "4"
                                                  ]
                                   }
                               )
    H.describe "parse full service" $ do
       -- H.it "empty headers" $ C.parse parse "[Unit]" `HPP.shouldBe` Right mempty
        H.it "empty headers"
            $              C.parse C.parser "[Unit]\n[Service]\n"
            `HPP.shouldBe` Right (mempty :: SystemdService)
        H.it "empty headers"
            $              C.parse C.parser "[Unit]\n\n\n[Service]\n\n"
            `HPP.shouldBe` Right (mempty :: SystemdService)
        H.it "with description"
            $ C.parse C.parser "[Unit]\n\nDescription=my service\n[Service]\n\n"
            `HPP.shouldBe` Right
                               ((mempty :: SystemdService)
                                   { unit = mempty
                                       { description = Value
                                           $ Description "my service"
                                       }
                                   }
                               )
        H.it "with description and type"
            $              C.parse
                               C.parser
                               "[Unit]\nDescription=my service\n[Service]\nType=simple\nExecStart=my cmd\n"
            `HPP.shouldBe` Right
                               ((mempty :: SystemdService)
                                   { unit    = mempty
                                       { description = Value
                                           $ Description "my service"
                                       }
                                   , service = mempty
                                       { type_ = TSimple
                                           (mempty { command = "my cmd" })
                                       }
                                   }
                               )
        H.it "with service"
            $              C.parse
                               C.parser
                               "[Service]\nUser=aria2\nGroup=aria2\nWorkingDirectory=/opt/AriaNg-1.1.1\nExecStart=/usr/bin/darkhttpd . --port 6810\n"
            `HPP.shouldBe` Right (mempty :: SystemdService)
                               { service = mempty
                                   { type_            =
                                       TSimple
                                           (mempty
                                               { command =
                                                   "/usr/bin/darkhttpd . --port 6810"
                                               }
                                           )
                                   , user             = Value $ User "aria2"
                                   , group            = Value $ Group "aria2"
                                   , workingDirectory = Value
                                       $ WorkingDirectory "/opt/AriaNg-1.1.1"
                                   }
                               }
        H.it "with service and tasksmax=10"
            $              C.parse
                               C.parser
                               "[Service]\nTasksMax=10\nExecStart=/usr/bin/darkhttpd . --port 6810\n"
            `HPP.shouldBe` Right (mempty :: SystemdService)
                               { service = mempty
                                   { type_    =
                                       TSimple
                                           (mempty
                                               { command =
                                                   "/usr/bin/darkhttpd . --port 6810"
                                               }
                                           )
                                   , tasksMax = Value $ TasksMax 10
                                   }
                               }
        H.it "with service and tasksmax=infinity"
            $              C.parse
                               C.parser
                               "[Service]\nTasksMax=infinity\nExecStart=/usr/bin/darkhttpd . --port 6810\n"
            `HPP.shouldBe` Right (mempty :: SystemdService)
                               { service = mempty
                                   { type_    =
                                       TSimple
                                           (mempty
                                               { command =
                                                   "/usr/bin/darkhttpd . --port 6810"
                                               }
                                           )
                                   , tasksMax = Value TasksMaxInfinity
                                   }
                               }
    H.describe "full-fledge services" $ do
        H.it "aria2 web service"
            $              C.parse
                               C.parser
                               [r|[Unit]
Description=Aria2 Web Service
After=network.target

[Service]
User=aria2
Group=aria2
WorkingDirectory=/opt/AriaNg-1.1.1
ExecStart=/usr/bin/darkhttpd . --port 6810

[Install]
WantedBy=default.target
|]
            `HPP.shouldBe` Right
                               ((mempty :: SystemdService)
                                   { unit    = mempty
                                       { description = Value
                                           $ Description "Aria2 Web Service"
                                       , after       = [Target "network.target"]
                                       }
                                   , service = mempty
                                       { user             = Value $ User "aria2"
                                       , group = Value $ Group "aria2"
                                       , workingDirectory =
                                           Value $ WorkingDirectory
                                               "/opt/AriaNg-1.1.1"
                                       , type_            =
                                           TSimple
                                               (mempty
                                                   { command =
                                                       "/usr/bin/darkhttpd . --port 6810"
                                                   }
                                               )
                                       }
                                   , install = mempty
                                       { wantedBy = [Target "default.target"]
                                       }
                                   }
                               )
        H.it "laptop mode tools"
            $              C.parse
                               C.parser
                               [r|[Unit]
Description=Laptop Mode Tools
Documentation=man:laptop_mode(8) man:laptop-mode.conf(8)
Documentation=http://github.com/rickysarraf/laptop-mode-tools

[Service]
Type=oneshot
RemainAfterExit=yes
ExecStartPre=/bin/rm -f /var/run/laptop-mode-tools/enabled
ExecStartPre=/bin/rm -f /var/run/laptop-mode-tools/state
ExecStart=/usr/bin/laptop_mode init force
ExecStop=/usr/bin/laptop_mode init stop
ExecStopPost=/bin/rm -f /var/run/laptop-mode-tools/enabled
ExecStopPost=/bin/rm -f /var/run/laptop-mode-tools/state
ExecReload=/usr/bin/laptop_mode auto
StandardOutput=journal
StandardError=journal
TasksMax=infinity

[Install]
WantedBy=multi-user.target
|]
            `HPP.shouldBe` Right
                               ((mempty :: SystemdService)
                                   { unit    = mempty
                                       { description   = Value
                                           $ Description "Laptop Mode Tools"
                                       , documentation =

                                           [ Documentation "man:laptop_mode(8)"
                                           , Documentation
                                               "man:laptop-mode.conf(8)"
                                           , Documentation
                                               "http://github.com/rickysarraf/laptop-mode-tools"
                                           ]
                                       }
                                   , service = mempty
                                       { type_           =
                                           TOneShot
                                               [ mempty
                                                     { command =
                                                         "/usr/bin/laptop_mode init force"
                                                     }
                                               ]
                                       , remainAfterExit = Value
                                           $ RemainAfterExit True
                                       , execStartPre    =
                                           [ mempty
                                               { command =
                                                   "/bin/rm -f /var/run/laptop-mode-tools/enabled"
                                               }
                                           , mempty
                                               { command =
                                                   "/bin/rm -f /var/run/laptop-mode-tools/state"
                                               }
                                           ]
                                       , execStop        =
                                           [ mempty
                                                 { command =
                                                     "/usr/bin/laptop_mode init stop"
                                                 }
                                           ]
                                       , execStopPost    =
                                           [ mempty
                                               { command =
                                                   "/bin/rm -f /var/run/laptop-mode-tools/enabled"
                                               }
                                           , mempty
                                               { command =
                                                   "/bin/rm -f /var/run/laptop-mode-tools/state"
                                               }
                                           ]
                                       , execReload      = Value $ mempty
                                           { command =
                                               "/usr/bin/laptop_mode auto"
                                           }
                                       , standardOutput  = Value OJournal
                                       , standardError   = Value OJournal
                                       , tasksMax = Value TasksMaxInfinity
                                       }
                                   , install = mempty
                                       { wantedBy = [Target "multi-user.target"]
                                       }
                                   }
                               )
        H.it "network time service"
            $              C.parse
                               C.parser
                               [r|[Unit]
Description=Network Time Service
After=network.target nss-lookup.target
Conflicts=systemd-timesyncd.service

[Service]
Type=forking
PrivateTmp=true
ExecStart=/usr/bin/ntpd -g -u ntp:ntp
Restart=always

[Install]
WantedBy=multi-user.target
|]
            `HPP.shouldBe` Right
                               ((mempty :: SystemdService)
                                   { unit    = mempty
                                       { description = Value
                                           $ Description "Network Time Service"
                                       , after = [ Target "network.target"
                                                 , Target "nss-lookup.target"
                                                 ]
                                       , conflicts =

                                           [Target "systemd-timesyncd.service"]
                                       }
                                   , service = mempty
                                       { type_      = TForking
                                           Empty
                                           (mempty
                                               { command =
                                                   "/usr/bin/ntpd -g -u ntp:ntp"
                                               }
                                           )
                                       , privateTmp = Value (PrivateTmp True)
                                       , restart    = Value RAlways
                                       }
                                   , install = mempty
                                       { wantedBy = [Target "multi-user.target"]
                                       }
                                   }
                               )
        H.it "networking for netctl profile %I"
            $              C.parse
                               C.parser
                               [r|[Unit]
Description=Networking for netctl profile %I
Documentation=man:netctl.profile(5)
After=network-pre.target
Before=network.target netctl.service
Wants=network.target

[Service]
Type=notify
NotifyAccess=exec
RemainAfterExit=yes
ExecStart=/usr/lib/netctl/network start %I
ExecStop=/usr/lib/netctl/network stop %I
|]
            `HPP.shouldBe` Right
                               ((mempty :: SystemdService)
                                   { unit    = mempty
                                       { description =
                                           Value
                                               $ Description
                                                     "Networking for netctl profile %I"
                                       , documentation =

                                           [ Documentation
                                                 "man:netctl.profile(5)"
                                           ]
                                       , after = [Target "network-pre.target"]
                                       , before = [ Target "network.target"
                                                  , Target "netctl.service"
                                                  ]
                                       , wants = [Target "network.target"]
                                       }
                                   , service = mempty
                                       { type_           = TNotify
                                           NAExec
                                           (mempty
                                               { command =
                                                   "/usr/lib/netctl/network start %I"
                                               }
                                           )
                                       , remainAfterExit = Value
                                           $ RemainAfterExit True
                                       , execStop        =
                                           [ mempty
                                                 { command =
                                                     "/usr/lib/netctl/network stop %I"
                                                 }
                                           ]
                                       }
                                   }
                               )

    H.describe "generate" $ do
        H.it "with wrong path"
            $ (C.generate $ C.fieldsTree [(C.Path ["hello"], "one")])
            `HPP.shouldBe` ( [C.UnknownPath (C.Path ["hello"])]
                           , Nothing :: Maybe SystemdService
                           )
        H.it "with description"
            $              (C.generate $ C.fieldsTree
                               [(C.Path ["Unit", "Description"], "my description")]
                           )

            `HPP.shouldBe` ( []
                           , Just $ mempty
                               { unit = mempty
                                   { description = Value
                                       $ Description "my description"
                                   }
                               }
                           )
        H.it "with description and after"
            $              (C.generate $ C.fieldsTree
                               [ (C.Path ["Unit", "Description"], "my description")
                               , (C.Path ["Unit", "After"], "target1")
                               , (C.Path ["Unit", "After"], "target2")
                               ]
                           )
            `HPP.shouldBe` ( []
                           , Just $ mempty
                               { unit = mempty
                                   { description = Value
                                       $ Description "my description"
                                   , after       = [ Target "target1"
                                                   , Target "target2"
                                                   ]
                                   }
                               }
                           )
        H.it "with install"
            $              (C.generate $ C.fieldsTree
                               [ (C.Path ["Install", "WantedBy"], "me")
                               , (C.Path ["Install", "RequiredBy"], "target1 target2")
                               ]
                           )
            `HPP.shouldBe` ( []
                           , Just $ mempty
                               { install = mempty
                                               { wantedBy   = [Target "me"]
                                               , requiredBy = [ Target "target1"
                                                              , Target "target2"
                                                              ]
                                               }
                               }
                           )
        H.it "with service"
            $              (C.generate $ C.fieldsTree
                               [ (C.Path ["Service", "User"]     , "me")
                               , (C.Path ["Service", "ExecStart"], "cmd")
                               ]
                           )
            `HPP.shouldBe` ( []
                           , Just $ mempty
                               { service = mempty
                                   { type_ = TSimple
                                                 (mempty { command = "cmd" })
                                   , user  = Value $ User "me"
                                   }
                               }
                           )

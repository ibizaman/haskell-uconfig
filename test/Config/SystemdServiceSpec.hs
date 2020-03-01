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
import qualified Parser                        as P


spec :: H.Spec
spec = do
    H.describe "parseExec" $ do
        H.it "parses empty command"
            $              P.parse parseExec ""
            `HPP.shouldBe` Right mempty
        H.it "parses simple command"
            $              P.parse parseExec "xyz"
            `HPP.shouldBe` Right (mempty { command = "xyz" })
        H.it "parses simple command with spaces"
            $              P.parse parseExec "x y z"
            `HPP.shouldBe` Right (mempty { command = "x y z" })
        H.it "parses command with options"
            $              P.parse parseExec "@-xy@-z"
            `HPP.shouldBe` Right
                               (mempty { command         = "xy@-z"
                                       , overrideName    = True
                                       , continueOnError = True
                                       }
                               )
    H.describe "parseService" $ do
        H.it "fails if no Type"
            $              P.parse parseService ""
            `HPP.shouldBe` Left
                               "input:1:1:\n  |\n1 | <empty line>\n  | ^\nExpected 'ExecStart' assignment for Type='simple'\n"
        H.it "fails if wrong Type"
            $              P.parse parseService "Type=other"
            `HPP.shouldBe` Left
                               "input:1:11:\n  |\n1 | Type=other\n  |           ^\nType with unknown value 'other'\n"
        H.it "fails if Type is right but no ExecStart"
            $              P.parse parseService "Type=simple"
            `HPP.shouldBe` Left
                               "input:1:12:\n  |\n1 | Type=simple\n  |            ^\nExpected 'ExecStart' assignment for Type='simple'\n"
        H.it "succeeds with Type and ExecStart"
            $              P.parse parseService "ExecStart=mycmd\nType=simple\n"
            `HPP.shouldBe` Right
                               (mempty
                                   { type_ = TSimple
                                                 $ mempty { command = "mycmd" }
                                   }
                               )
        H.it "succeeds with Type and ExecStart"
            $ P.parse parseService "Type=simple\n\nExecStart=my cmd\n[other]"
            `HPP.shouldBe` Right
                               (mempty
                                   { type_ = TSimple
                                                 $ mempty { command = "my cmd" }
                                   }
                               )
        H.it "succeeds with Type and ExecStart with spaces"
            $ P.parse parseService "Type=simple\n\nExecStart=m y cm d\n[other]"
            `HPP.shouldBe` Right
                               (mempty
                                   { type_ = TSimple $ mempty
                                                 { command = "m y cm d"
                                                 }
                                   }
                               )
        H.it "succeeds with ExecStart before Type"
            $ P.parse parseService "\nExecStart=mycmd\n\nType=simple\n[other]"
            `HPP.shouldBe` Right
                               (mempty
                                   { type_ = TSimple
                                                 $ mempty { command = "mycmd" }
                                   }
                               )
        H.it "succeeds with ExecStart with spaces before Type"
            $ P.parse parseService
                      "\nExecStart=m y cm d\n\nType=simple\n[other]"
            `HPP.shouldBe` Right
                               (mempty
                                   { type_ = TSimple $ mempty
                                                 { command = "m y cm d"
                                                 }
                                   }
                               )
    H.describe "parseUnit" $ do
        H.it "with before"
            $ P.parse parseUnit "Before=one two\nBefore=three\n[Service]\n\n"
            `HPP.shouldBe` Right (mempty { before = ["one", "two", "three"] })
        H.it "with description"
            $ P.parse parseUnit "\n\nDescription=my service\n[Service]\n\n"
            `HPP.shouldBe` Right (mempty { description = "my service" })
    H.describe "parseInstall" $ do
        H.it "empty"
            $              P.parse parseUnit "\n[Service]\n\n"
            `HPP.shouldBe` Right mempty
        H.it "wantedBy"
            $ P.parse parseInstall "\nWantedBy=default.target\n\n[Service]\n\n"
            `HPP.shouldBe` Right (mempty { wantedBy = ["default.target"] })
        H.it "wantedBy and requiredBy"
            $              P.parse
                               parseInstall
                               "\nWantedBy=default.target\n\nRequiredBy=other.target\n\nRequiredBy=4\nWantedBy=1 2 3\n[Service]\n\n"
            `HPP.shouldBe` Right
                               (mempty
                                   { wantedBy   = [ "default.target"
                                                  , "1"
                                                  , "2"
                                                  , "3"
                                                  ]
                                   , requiredBy = ["other.target", "4"]
                                   }
                               )
    H.describe "parse full service" $ do
       -- H.it "empty headers" $ P.parse parse "[Unit]" `HPP.shouldBe` Right mempty
        H.it "empty headers"
            $              P.parse parse "[Unit]\n[Service]\n"
            `HPP.shouldBe` Right mempty
        H.it "empty headers"
            $              P.parse parse "[Unit]\n\n\n[Service]\n\n"
            `HPP.shouldBe` Right mempty
        H.it "with description"
            $ P.parse parse "[Unit]\n\nDescription=my service\n[Service]\n\n"
            `HPP.shouldBe` Right
                               (mempty
                                   { unit = mempty { description = "my service"
                                                   }
                                   }
                               )
        H.it "with description and type"
            $              P.parse
                               parse
                               "[Unit]\nDescription=my service\n[Service]\nType=simple\nExecStart=my cmd\n"
            `HPP.shouldBe` Right
                               (mempty
                                   { unit = mempty { description = "my service"
                                                   }
                                   , service =
                                       mempty
                                           { type_ =
                                               TSimple
                                                   (mempty { command = "my cmd"
                                                           }
                                                   )
                                           }
                                   }
                               )
        H.it "with service"
            $              P.parse
                               parse
                               "[Service]\nUser=aria2\nGroup=aria2\nWorkingDirectory=/opt/AriaNg-1.1.1\nExecStart=/usr/bin/darkhttpd . --port 6810\n"
            `HPP.shouldBe` Right mempty
                               { service =
                                   mempty
                                       { type_            =
                                           TSimple
                                               (mempty
                                                   { command =
                                                       "/usr/bin/darkhttpd . --port 6810"
                                                   }
                                               )
                                       , user             = Value "aria2"
                                       , group            = Value "aria2"
                                       , workingDirectory =
                                           Value "/opt/AriaNg-1.1.1"
                                       }
                               }
        H.it "with service and tasksmax=10"
            $              P.parse
                               parse
                               "[Service]\nTasksMax=10\nExecStart=/usr/bin/darkhttpd . --port 6810\n"
            `HPP.shouldBe` Right mempty
                               { service =
                                   mempty
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
            $              P.parse
                               parse
                               "[Service]\nTasksMax=infinity\nExecStart=/usr/bin/darkhttpd . --port 6810\n"
            `HPP.shouldBe` Right mempty
                               { service =
                                   mempty
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
            $              P.parse
                               parse
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
                               (mempty
                                   { unit    = mempty
                                                   { description =
                                                       "Aria2 Web Service"
                                                   , after = ["network.target"]
                                                   }
                                   , service =
                                       mempty
                                           { user             = Value "aria2"
                                           , group            = Value "aria2"
                                           , workingDirectory =
                                               Value "/opt/AriaNg-1.1.1"
                                           , type_            =
                                               TSimple
                                                   (mempty
                                                       { command =
                                                           "/usr/bin/darkhttpd . --port 6810"
                                                       }
                                                   )
                                           }
                                   , install = mempty
                                                   { wantedBy =
                                                       ["default.target"]
                                                   }
                                   }
                               )
        H.it "aria2 web service"
            $              P.parse
                               parse
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
                               (mempty
                                   { unit    =
                                       mempty
                                           { description   = "Laptop Mode Tools"
                                           , documentation =
                                               [ "man:laptop_mode(8)"
                                               , "man:laptop-mode.conf(8)"
                                               , "http://github.com/rickysarraf/laptop-mode-tools"
                                               ]
                                           }
                                   , service =
                                       mempty
                                           { type_          =
                                               TOneShot
                                                   (RemainAfterExit True)
                                                   [ mempty
                                                         { command =
                                                             "/usr/bin/laptop_mode init force"
                                                         }
                                                   ]
                                           , execStartPre   =
                                               [ mempty
                                                   { command =
                                                       "/bin/rm -f /var/run/laptop-mode-tools/enabled"
                                                   }
                                               , mempty
                                                   { command =
                                                       "/bin/rm -f /var/run/laptop-mode-tools/state"
                                                   }
                                               ]
                                           , execStop       =
                                               [ mempty
                                                     { command =
                                                         "/usr/bin/laptop_mode init stop"
                                                     }
                                               ]
                                           , execStopPost   =
                                               [ mempty
                                                   { command =
                                                       "/bin/rm -f /var/run/laptop-mode-tools/enabled"
                                                   }
                                               , mempty
                                                   { command =
                                                       "/bin/rm -f /var/run/laptop-mode-tools/state"
                                                   }
                                               ]
                                           , execReload     =
                                               Value $ mempty
                                                   { command =
                                                       "/usr/bin/laptop_mode auto"
                                                   }
                                           , standardOutput = OJournal
                                           , standardError  = OJournal
                                           , tasksMax = Value TasksMaxInfinity
                                           }
                                   , install = mempty
                                                   { wantedBy =
                                                       ["multi-user.target"]
                                                   }
                                   }
                               )

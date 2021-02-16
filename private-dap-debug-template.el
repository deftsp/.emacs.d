(dap-register-debug-template "tradingbot-rs::LLDB Run Configuration"
                             (list :type "lldb"
                                   :request "launch"
                                   :name "LLDB::Run"
                                   :cwd (expand-file-name "~/Lab/tradingbot-rs")
                                   :target "target/debug/tradingbot"
                                   :gdbpath "rust-lldb"))

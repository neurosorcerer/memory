--DISPATCH to 'These' Functions:

module DispatchSection where 

dispatch :: String -> [String] -> IO ()
dispatch "help"                 = help 
dispatch "clear"                = clear
dispatch "memory_help"          = memory_help
dispatch "memory_add"           = memory_add
dispatch "memory_view"          = memory_view
dispatch "memory_search"        = memory_search
dispatch "memory_focus"         = memory_focus
dispatch "voice_help"           = voice_help
dispatch "voice_add"            = voice_add
dispatch "voice_view"           = voice_view
dispatch "voice_search"         = voice_search
dispatch "voice_focus"          = voice_focus
dispatch "heartRate_help"       = heartRate_help
dispatch "heartRate_add"        = heartRate_add
dispatch "heartRate_search"     = heartRate_search
dispatch "heartRate_stats"      = heartRate_stats
dispatch "wordsort"             = wordsort
dispatch "searchWS"             = searchWS
dispatch "internalhelp"         = internalhelp
dispatch command  = doesntExist command

h_input args     = if null args then [] else head args
t_input args     = if null args then [] else tail args



--























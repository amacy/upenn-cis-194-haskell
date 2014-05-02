{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

parseMessage :: String -> LogMessage
parseMessage ('I':' ':int:' ':msg) = LogMessage MessageType Info int msg
parseMessage ('W':' ':int:' ':msg) = LogMessage MessageType Warning int msg
parseMessage ('E':' ':int:' ':msg) = LogMessage MessageType Error Int int msg
parseMessage msg                   = LogMessage msg



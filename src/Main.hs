import qualified Data.ByteString.Lazy as BL
import System.Console.ANSI
import System.Console.GetOpt
import System.Environment
import System.Exit
import Control.Monad
import Data.Word
import Numeric


(|>) :: a -> (a -> b) -> b
x |> f = f x

main = do
    args <- getArgs
    let (actions, nonOptions, errors ) = getOpt RequireOrder options args
    opts <- foldl (>>=) (return startOptions) actions
    let Options { optInput = input
                , processInput = proc
                , find = find
                , optOutput = output } = opts
    input >>= output.hexDump find.proc
    BL.pack [0x1b,0x5b, 0x31, 0x3b, 0x33, 0x33, 0x6d] |> BL.writeFile "test.bin"
    BL.pack [0x1b,0x5b, 0x30, 0x6d] |> BL.writeFile "reset.bin"


data Options = Options  { optInput :: IO BL.ByteString
                        , processInput :: BL.ByteString -> BL.ByteString
                        , find :: [(BL.ByteString, SGR)]
                        , optOutput :: String -> IO()
                        }


startOptions = Options  { optInput = BL.getContents
                        , processInput = id
                        , find = []
                        , optOutput = print
                        }
    
options :: [ OptDescr ( Options -> IO Options ) ]
options =
    [ Option "i" ["input"]
        (ReqArg
            (\arg opt -> return opt { optInput = BL.readFile arg })
            "FILE")
        "Input file"
    , Option "o" ["output"]
        (ReqArg
            (\arg opt -> return opt { optOutput = writeFile arg })
            "FILE")
        "Output file"

    , Option "d" ["drop"]
        (ReqArg
            (\arg opt -> do
                let Options { processInput = inp } = opt
                return opt { processInput = (BL.drop ((read arg :: Int) |> fromIntegral)).inp })
            "INT")
        "Drop n bytes"
    , Option "t" ["take"]
        (ReqArg
            (\arg opt -> do
                let Options { processInput = inp } = opt
                return opt { processInput = (BL.take ((read arg :: Int) |> fromIntegral)).inp })
            "INT")
        "Take n bytes"

    , Option "v" ["version"]
        (NoArg
            (\_ -> version >> exit))
        "Print version"
    , Option "h" ["help"]
        (NoArg
            (\_ -> usage >> exit))
        "Show help"
    ]


parse ["-h"] = usage >> exit
parse ["--help"] = parse ["-h"]
parse ["-v"] = version >> exit
parse ["--version"] = parse ["-v"]
parse [] = 
    setSGR [SetColor Foreground Vivid Red] >>
        BL.getContents >>= printHex >>
        setSGR [Reset]
    where
        printHex = sequence.map print.BL.unpack  


usage = do
    prg <- getProgName
    putStrLn (usageInfo prg options)
--putStrLn "Usage: hex-find [-vh] "
version = putStrLn "Haskell hex-find 0.1"
exit = exitWith ExitSuccess


hexDump :: [(BL.ByteString, SGR)] -> BL.ByteString -> String
hexDump _ = concat.map (flip showHex " ").BL.unpack
-- hexDump d cols = d |> splitAt cols |> map (\(x,y) -> [hexLine x cols, hexDump y cols]) 

-- hexLine d cols = 

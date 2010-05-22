-----------------------------------------------------------------------------
-- |
-- Module      :  System.Console.GetFlag
-- Copyright   :  (c) Troels Henriksen 2010
--                (c) Sven Panne 2002-2005
-- License     :  MIT-style (see LICENSE)
-- Stability   :  stable
-- Portability :  portable
--
-- This adaptation of "System.Console.GetOpt" provides easy handling
-- of classic Unix/Plan 9-style command line options.  This means that
-- single dashes are used to prefix option names, which may be of any
-- length (not just single characters, although such is generally
-- recommended).  It is not possible to collapse multiple options in a
-- single parameter, so you will have to write @-a -b@ instead of
-- @-ab@.  A single GNU extension is included: the parameter @--@ will
-- cause anything past it to be returned as arguments, not options,
-- even if the parameters have leading dashes.
--
-- The API is almost compatible with "System.Console.GetOpt", the only
-- difference being in 'OptDescr', which no longer permits multiple
-- names per option.
--
-----------------------------------------------------------------------------

module System.Console.GetFlag (
   -- * GetFlag
   getOpt, getOpt',
   usageInfo,
   ArgOrder(..),
   OptDescr(..),
   ArgDescr(..),

   -- * Examples

   -- |To hopefully illuminate the role of the different data structures,
   -- here are the command-line options for a (very simple) compiler,
   -- done in two different ways.
   -- The difference arises because the type of 'getOpt' is
   -- parameterized by the type of values derived from flags.

   -- ** Interpreting flags as concrete values
   -- $example1

   -- ** Interpreting flags as transformations of an options record
   -- $example2
) where

import Prelude

-- |What to do with options following non-options
data ArgOrder a
    = RequireOrder                -- ^ no option processing after first non-option
    | Permute                     -- ^ freely intersperse options and non-options
    | ReturnInOrder (String -> a) -- ^ wrap non-options into options

{-|
Each 'OptDescr' describes a single option.

The arguments to 'Option' are:

* the name of the option

* argument descriptor

* explanation of option for user
-}
data OptDescr a =      -- description of a single options:
    Option String         --    option string
    (ArgDescr a)          --    argument descriptor
    String                --    explanation of option for user

-- |Describes whether an option takes an argument or not, and if so
-- how the argument is injected into a value of type @a@.
data ArgDescr a
    = NoArg                   a         -- ^   no argument expected
    | ReqArg (String       -> a) String -- ^   option requires argument
    | OptArg (Maybe String -> a) String -- ^   optional argument

data OptKind a                -- kind of cmd line arg (internal use only):
    = Opt       a                --    an option
    | UnreqOpt  String           --    an un-recognized option
    | NonOpt    String           --    a non-option
    | EndOfOpts                  --    end-of-options marker (i.e. "--")
    | OptErr    String           --    something went wrong...

-- | Return a string describing the usage of a command, derived from
-- the header (first argument) and the options described by the 
-- second argument.
usageInfo :: String                    -- header
          -> [OptDescr a]              -- option descriptors
          -> String                    -- nicely formatted decription of options
usageInfo header optDescr = unlines (header:table)
    where (ss,ds)        = (unzip . concatMap fmtOpt) optDescr
          table          = zipWith paste (sameLen ss) ds
          paste x y      = "  " ++ x ++ "  " ++ y
          sameLen xs     = flushLeft ((maximum . map length) xs) xs
          flushLeft n xs = [ take n (x ++ repeat ' ') | x <- xs ]

fmtOpt :: OptDescr a -> [(String,String)]
fmtOpt (Option name ad descr) =
  case lines descr of
    []     -> [(fmt ad,"")]
    (d:ds) ->  (fmt ad,d) : [ ("",d') | d' <- ds ]
    where fmt (NoArg  _   )  = '-' : name
          fmt (ReqArg _ ad') = '-' : name ++ " " ++ ad'
          fmt (OptArg _ ad') = '-' : name ++ "[" ++ ad' ++ "]"

{-|
Process the command-line, and return the list of values that matched
(and those that didn\'t). The arguments are:

* The order requirements (see 'ArgOrder')

* The option descriptions (see 'OptDescr')

* The actual command line arguments (presumably got from 
  'System.Environment.getArgs').

'getOpt' returns a triple consisting of the option arguments, a list
of non-options, and a list of error messages.
-}
getOpt :: ArgOrder a                   -- non-option handling
       -> [OptDescr a]                 -- option descriptors
       -> [String]                     -- the command-line arguments
       -> ([a],[String],[String])      -- (options,non-options,error messages)
getOpt ordering optDescr args = (os,xs,es ++ map errUnrec us)
    where (os,xs,us,es) = getOpt' ordering optDescr args

{-|
This is almost the same as 'getOpt', but returns a quadruple
consisting of the option arguments, a list of non-options, a list of
unrecognized options, and a list of error messages.
-}
getOpt' :: ArgOrder a                         -- non-option handling
        -> [OptDescr a]                       -- option descriptors
        -> [String]                           -- the command-line arguments
        -> ([a],[String], [String] ,[String]) -- (options,non-options,unrecognized,error messages)
getOpt' _        _        []         =  ([],[],[],[])
getOpt' ordering optDescr (arg:args) = procNextOpt opt ordering
    where procNextOpt (Opt o)      _                 = (o:os,xs,us,es)
          procNextOpt (UnreqOpt u) _                 = (os,xs,u:us,es)
          procNextOpt (NonOpt x)   RequireOrder      = ([],x:rest,[],[])
          procNextOpt (NonOpt x)   Permute           = (os,x:xs,us,es)
          procNextOpt (NonOpt x)   (ReturnInOrder f) = (f x :os, xs,us,es)
          procNextOpt EndOfOpts    RequireOrder      = ([],rest,[],[])
          procNextOpt EndOfOpts    Permute           = ([],rest,[],[])
          procNextOpt EndOfOpts    (ReturnInOrder f) = (map f rest,[],[],[])
          procNextOpt (OptErr e)   _                 = (os,xs,us,e:es)

          (opt,rest) = getNext arg args optDescr
          (os,xs,us,es) = getOpt' ordering optDescr rest

-- take a look at the next cmd line arg and decide what to do with it
getNext :: String -> [String] -> [OptDescr a] -> (OptKind a,[String])
getNext ('-':'-':[]) rest _    = (EndOfOpts,rest)
getNext ('-':xs) rest optDescr = handleOpt xs rest optDescr
getNext arg      rest _        = (NonOpt arg,rest)

handleOpt :: String -> [String] -> [OptDescr a] -> (OptKind a,[String])
handleOpt ls rs optDescr = opt ads rs
    where options = [ o | o@(Option name _ _) <- optDescr
                        , ls == name ]
          ads     = [ ad | Option _ ad _ <- options ]
          optStr  = '-':ls

          opt [NoArg  a  ] rest     = (Opt a,rest)
          opt [ReqArg _ d] []       = (errReq d optStr,[])
          opt [ReqArg f _] (r:rest) = (Opt (f r),rest)
          opt [OptArg f _] rest     = (Opt (f Nothing),rest)
          opt []           rest     = (UnreqOpt optStr,rest)
          opt _            rest     = (errAmbig options optStr,rest)

-- miscellaneous error formatting

errAmbig :: [OptDescr a] -> String -> OptKind a
errAmbig ods optStr = OptErr (usageInfo header ods)
    where header = "option `" ++ optStr ++ "' is ambiguous; could be one of:"

errReq :: String -> String -> OptKind a
errReq d optStr = OptErr ("option `" ++ optStr ++ "' requires an argument " ++ d ++ "\n")

errUnrec :: String -> String
errUnrec optStr = "unrecognized option `" ++ optStr ++ "'\n"

{-
-----------------------------------------------------------------------------------------
-- and here a small and hopefully enlightening example:

data Flag = Verbose | Version | Name String | Output String | Arg String   deriving Show

options :: [OptDescr Flag]
options =
   [Option "ve" (NoArg Verbose)      "verbosely list files",
    Option "v"  (NoArg Version)      "show version info",
    Option "o"  (OptArg out "FILE")  "use FILE for dump",
    Option "n"  (ReqArg Name "USER") "only dump USER's files"]

out :: Maybe String -> Flag
out Nothing  = Output "stdout"
out (Just o) = Output o

test :: ArgOrder Flag -> [String] -> String
test order cmdline = case getOpt order options cmdline of
                        (o,n,[]  ) -> "options=" ++ show o ++ "  args=" ++ show n ++ "\n"
                        (_,_,errs) -> concat errs ++ usageInfo header options
   where header = "Usage: foobar [OPTION...] files..."

-- example runs:
-- putStr (test RequireOrder ["foo","-v"])
--    ==> options=[]  args=["foo", "-v"]
-- putStr (test Permute ["foo","-v"])
--    ==> options=[Verbose]  args=["foo"]
-- putStr (test (ReturnInOrder Arg) ["foo","-v"])
--    ==> options=[Arg "foo", Verbose]  args=[]
-- putStr (test Permute ["foo","--","-v"])
--    ==> options=[]  args=["foo", "-v"]
-----------------------------------------------------------------------------------------
-}

{- $example1

A simple choice for the type associated with flags is to define a type
@Flag@ as an algebraic type representing the possible flags and their
arguments:

>    module Opts1 where
>    
>    import System.Console.GetOpt
>    import Data.Maybe ( fromMaybe )
>    
>    data Flag 
>     = Verbose  | Version 
>     | Input String | Output String | LibDir String
>       deriving Show
>    
>    options :: [OptDescr Flag]
>    options =
>     [ Option "verbose" (NoArg Verbose)       "chatty output on stderr"
>     , Option "v"       (NoArg Version)       "show version number"
>     , Option "o"       (OptArg outp "FILE")  "output FILE"
>     , Option "c"       (OptArg inp  "FILE")  "input FILE"
>     , Option "L"       (ReqArg LibDir "DIR") "library directory"
>     ]
>    
>    inp,outp :: Maybe String -> Flag
>    outp = Output . fromMaybe "stdout"
>    inp  = Input  . fromMaybe "stdin"
>    
>    compilerOpts :: [String] -> IO ([Flag], [String])
>    compilerOpts argv = 
>       case getOpt Permute options argv of
>          (o,n,[]  ) -> return (o,n)
>          (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
>      where header = "Usage: ic [OPTION...] files..."

Then the rest of the program will use the constructed list of flags
to determine it\'s behaviour.

-}

{- $example2

A different approach is to group the option values in a record of type
@Options@, and have each flag yield a function of type
@Options -> Options@ transforming this record.

>    module Opts2 where
>
>    import System.Console.GetOpt
>    import Data.Maybe ( fromMaybe )
>
>    data Options = Options
>     { optVerbose     :: Bool
>     , optShowVersion :: Bool
>     , optOutput      :: Maybe FilePath
>     , optInput       :: Maybe FilePath
>     , optLibDirs     :: [FilePath]
>     } deriving Show
>
>    defaultOptions    = Options
>     { optVerbose     = False
>     , optShowVersion = False
>     , optOutput      = Nothing
>     , optInput       = Nothing
>     , optLibDirs     = []
>     }
>
>    options :: [OptDescr (Options -> Options)]
>    options =
>     [ Option "verbose"
>         (NoArg (\ opts -> opts { optVerbose = True }))
>         "chatty output on stderr"
>     , Option "v"
>         (NoArg (\ opts -> opts { optShowVersion = True }))
>         "show version number"
>     , Option "o"
>         (OptArg ((\ f opts -> opts { optOutput = Just f }) . fromMaybe "output")
>                 "FILE")
>         "output FILE"
>     , Option "c"
>         (OptArg ((\ f opts -> opts { optInput = Just f }) . fromMaybe "input")
>                 "FILE")
>         "input FILE"
>     , Option "L"
>         (ReqArg (\ d opts -> opts { optLibDirs = optLibDirs opts ++ [d] }) "DIR")
>         "library directory"
>     ]
>
>    compilerOpts :: [String] -> IO (Options, [String])
>    compilerOpts argv =
>       case getOpt Permute options argv of
>          (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
>          (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
>      where header = "Usage: ic [OPTION...] files..."

Similarly, each flag could yield a monadic function transforming a record,
of type @Options -> IO Options@ (or any other monad), allowing option
processing to perform actions of the chosen monad, e.g. printing help or
version messages, checking that file arguments exist, etc.

-}

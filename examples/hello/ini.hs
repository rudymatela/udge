
function :: [a] -> [a]
function  =  undefined

main :: IO ()
main  =  putStrLn "Hello, World!"

{-

This is a simple a Haskell source file
that the "file" command misidentifies as an INI file:

    $ file program.hs
    ini.hs: Generic INItialization configuration [a]

This comment is no necessary for misidentification.
A near-minimal example is:

CRLFf::[a]->[a]CRLF

Above, CRLF is the "DOS/Windows" line break.
Starting with a blank line is necessary for misidentification.

Apparently "file" misconstrues [a], [Int] or [Etc] as INI-file section markers.
Even if these appear further down in the file, misidentification still happens.

As of file v5.45 (2023), there is no "magic" for Haskell files,
so they are generally identified as:

    $ file hello.hs
    hello.hs: ASCII text

Within Udge, this causes submissions to receive a "compile error" message,
as the compile-and-run-1 script uses file before moving forward.
(Many thanks to Elias Dovkrans for discovering and reporting this issue.)

A simple fix is to use the -esoft flag, which will not use magic files
thus will simply identify the program as a plain text file:

    $ file ini.hs
    ini.hs: ASCII text, with CRLF line terminators

This is file serves as a regression-test for Udge.

(C) 2023  Rudy Matela
This text is available under the CC BY-SA 4.0 license
or (at your option) the GFDL 1.3 license
or (at your option) the GNU GPLv2 license or later.

-}

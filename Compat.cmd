@setlocal
@echo off

:: This can be done as (slightly long) one-liner, but embedded in Makefile it
:: encodes the knowledge that GNU make uses batch files internally, which is a
:: bit brittle. findstr /N is a trick to allow blank lines to be included. More
:: subtly, it allows tagged blank lines to be detected.
for /f "tokens=1* delims=:" %%a in ('findstr /N /R "^" %2') do (
  rem Handle blank lines in the file
  if "%%b" equ "" (
    echo.
  ) else (
    for /f "tokens=1* delims=@" %%l in ("%%b") do (
      rem If %%l == %%b then the line didn't contain @ - we already know the
      rem line isn't blank.
      if "%%l" equ "%%b" (
        echo %%l
      ) else (
        if %1 lss %%l00 (
          if "%%m" equ "" (
            echo.
          ) else (
            echo %%m
          )
        )
      )
    )
  )
)

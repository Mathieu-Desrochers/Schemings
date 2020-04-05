date
----
A record with the following fields.

- year
- month
- day

date-time
---------
A record with the following fields.

- year
- month
- day
- hour
- minute
- second

time
----
A record with the following fields.

- hour
- minute
- second

date-today
----------
Returns the current UTC date.

date-time-now
-------------
Returns the current UTC date time.

time-now
--------
Returns the current UTC time.

date-week-day
-------------
Returns the weekday of a date.

__date__  
A date.

__result__  
The weekday.

date-add
--------
Adds days to a date.

__date__  
A date.

__days__  
A number of days.

__result__  
The new date.

date-add-business-days
----------------------
Adds business days to a date.

__date__  
A date.

__days__  
A number of days.

__holiday-dates__  
A list of holiday dates.

__result__  
The new date.

date-time-add
-------------
Adds seconds to a date time.

__date-time__  
A date time.

__seconds__  
A number of seconds.

__result__  
The new date time.

time-add
--------
Adds seconds to a time.

__time__  
A time.

__seconds__  
A number of seconds.

__result__  
The new time.

date-diff
---------
Returns the number of days between two dates.

__date-from__  
A from date.

__date-to__  
A to date.

__result__  
The number of days.

date-time-diff
--------------
Returns the number of seconds between two date times.

__date-time-from__  
A from date time.

__date-time-to__  
A to date time.

__result__  
The number of seconds.

time-diff
---------
Returns the number of seconds between two times.

__time-from__  
A from time.

__time-to__  
A to time.

__result__  
The number of seconds.

string->date
------------
Parses a date string.  
Uses the "%Y-%m-%d" format.

__string__  
A date string.

__result__  
The parsed date.

string->date-time
-----------------
Parses a date time string.  
Uses the "%Y-%m-%dT%H:%M:%SZ" format.

__string__  
A date time string.

__result__  
The parsed date time.

string->time*
-------------
Parses a time string.  
Uses the "%H:%M:%S" format.  
Named to avoid clashes with the posix unit.

__string__  
A time string.

__result__  
The parsed time.

try-string->date
----------------
Tries to parse a date string.  
Uses the "%Y-%m-%d" format.

__string__  
A date string.

__result__  
Returns #f if the string does not represent a valid date.  
Otherwise the parsed date.

try-string->date-time
---------------------
Tries to parse a date time string.  
Uses the "%Y-%m-%dT%H:%M:%SZ" format.

__string__  
A date time string.

__result__  
Returns #f if the string does not represent a valid date time.  
Otherwise the parsed date time.

try-string->time
----------------
Tries to parse a time string.  
Uses the "%H:%M:%S" format.

__string__  
A time string.

__result__  
Returns #f if the string does not represent a valid time.  
Otherwise the parsed time.

date->string
------------
Formats a date.  
Uses the "%Y-%m-%d" format.

__date__  
A date.

__result__  
The formatted date.

date-time->string
-----------------
Formats a date time.  
Uses the "%Y-%m-%dT%H:%M:%SZ" format.

__date__  
A date time

__result__  
The formatted date time.

time->string*
-------------
Formats a time.  
Uses the "%H:%M:%S" format.  
Named to avoid clashes with the posix unit.

__date__  
A time

__result__  
The formatted time.

try it
------
Place the following code in sources/main.scm.

    (declare (uses date-time))

    (display (date->string (date-today)))
    (newline)

    (display
      (date-time-diff
        (string->date-time "2001-01-01T00:00:00Z")
        (string->date-time "2001-01-01T00:09:00Z")))
    (newline)

    (display
      (time->string*
        (time-add (make-time 6 0 0) 1262)))
    (newline)

Run the following commands.

    $ make
    $ ./main

    2019-05-13
    540
    06:21:02

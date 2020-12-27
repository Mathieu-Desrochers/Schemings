monitoring-initialize
---------------------
Initializes the monitoring.

Simply point to a statsd server and automatically  
gain timings on all the services and table operations.

__ip-address__  
A statsd server ip address.

__port-number__  
A statsd server port number.

monitoring-signal
-----------------
Signals an event.

__event-name__  
An event name.

monitoring-set-value
--------------------
Sets a monitored value.

__name__  
A value name.

__value__  
A value.

monitoring-timing
-----------------
Reports a timed operation.

__operation-name__  
An operation name.

__duration__  
A duration.

with-monitoring-timing
----------------------
Invokes a procedure and reports its timing.

__procedure-name__  
An procedure name.

__procedure__  
A procedure.

try it
------
Place the following code in sources/main.scm.

    (declare (uses monitoring))

    (monitoring-initialize "127.0.0.1" 8125)

    (monitoring-signal
      "main-started")

    (with-monitoring-timing
      "main-procedure"
      (lambda ()
        (sleep 2)))

Run the following commands.

    $ make
    $ ./main

powered by
----------
The great statsd-c-client.

with-monitoring
---------------
Invokes a procedure with monitoring.

__ip-address__  
A statsd server ip address.

__port-number__  
A statsd server port number.

__procedure__  
A procedure invoked with a monitor.

monitoring-signal
-----------------
Signals an event.

__monitor__  
A monitor.

__event-name__  
An event name.

monitoring-set-value
--------------------
Sets a monitored value.

__monitor__  
A monitor.

__name__  
A value name.

__value__  
A value.

monitoring-timing
-----------------
Reports a timed operation.

__monitor__  
A monitor.

__operation-name__  
An operation name.

__duration__  
A duration.

with-monitoring-timing
----------------------
Invokes a procedure and reports its timing.

__monitor__  
A monitor.

__procedure-name__  
An procedure name.

__procedure__  
A procedure.

try it
------
Place the following code in sources/main.scm.

    (declare (uses monitoring))

    (with-monitoring "127.0.0.1" 8125
      (lambda (monitor)

        (monitoring-signal monitor
          "main-started")

        (with-monitoring-timing monitor
          "main-procedure"
          (lambda ()
            (sleep 2)))))

Run the following commands.

    $ make
    $ ./main

powered by
----------
The great statsd-c-client.

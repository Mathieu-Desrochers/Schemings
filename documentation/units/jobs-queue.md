jobs-queue-start
----------------
Starts a jobs queue.  
This is a blocking call.

The queue acts as a well known point for jobs management.  
It accepts jobs from connected clients, and it distributes  
them fairly to the connected workers.

__submit-endpoint__  
An endpoint to which clients submit jobs.

__worker-endpoint__  
An endpoint from which workers receive jobs.

with-jobs-queue-connection
--------------------------
Invokes a procedure with a jobs queue connection.

__submit-endpoint__  
An endpoint to which to submit jobs.

__procedure__  
A procedure invoked with the jobs queue connection.

jobs-queue-submit
-----------------
Submits a job to a queue.  
This is a non-blocking call.

__jobs-queue-connection__  
A jobs queue connection.

__u8vector__  
A binary vector representing the job.

jobs-queue-receive
------------------
Invokes a procedure with jobs received from a queue.  
This is a blocking call.

__worker-endpoint__  
An endpoint from which to receive jobs.

__procedure__  
A procedure invoked with the jobs.

try it
------
Place the following code in sources/main.scm.

    (declare (uses jobs-queue))

    (jobs-queue-start
      "ipc:///tmp/jobs-queue.ipc"
      "ipc:///tmp/jobs-worker.ipc")

Run the following commands.

    $ make
    $ mv main main-queue

Place the following code in sources/main.scm.

    (import srfi-4)
    (import srfi-18)

    (declare (uses jobs-queue))

    (with-jobs-queue-connection
      "ipc:///tmp/jobs-queue.ipc"
      (lambda (jobs-queue-connection)

        (jobs-queue-submit jobs-queue-connection (make-u8vector 1 0))
        (thread-sleep! 1)

        (jobs-queue-submit jobs-queue-connection (make-u8vector 1 0))
        (thread-sleep! 1)

        (jobs-queue-submit jobs-queue-connection (make-u8vector 1 0))))

Run the following commands.

    $ make
    $ mv main main-client

Place the following code in sources/main.scm.

    (import srfi-18)

    (declare (uses debug))
    (declare (uses jobs-queue))

    (jobs-queue-receive
      "ipc:///tmp/jobs-worker.ipc"
      (lambda (u8vector length)
        (debug-print "working...")
        (thread-sleep! 5)
        (debug-print "done")))

Run the following commands.

    $ make
    $ mv main main-worker

Run the following command in a first terminal.

    $ ./main-queue

Run the following command in a second and third terminal.

    $ ./main-worker

Run the following command in a fourth terminal.

    $ ./main-client

powered by
----------
The great zeromq.

#include <zmq.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <assert.h>

int main(void)
{
    void* context = zmq_ctx_new();
    void* puller = zmq_socket(context, ZMQ_PULL);
    int rc = zmq_bind(puller, "ipc://jobs-queue.ipc");
    assert(rc == 0);

    void* pusher = zmq_socket(context, ZMQ_PUSH);
    rc = zmq_bind(pusher, "ipc://jobs-dispatch.ipc");
    assert(rc == 0);

    while (1)
    {
        char buffer[100];
        zmq_recv(puller, buffer, 100, 0);
        printf("received: %s\n", buffer);
        zmq_send(pusher, buffer, strlen(buffer), 0);
    }

    zmq_close(pusher);
    zmq_close(puller);
    zmq_ctx_destroy(context);

    return 0;
}

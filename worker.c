#include <zmq.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <assert.h>

int main(void)
{
    void* context = zmq_ctx_new();
    void* puller = zmq_socket(context, ZMQ_PULL);
    int rc = zmq_connect(puller, "ipc://jobs-dispatch.ipc");
    assert(rc == 0);

    while (1)
    {
        char buffer[100];
        zmq_recv(puller, buffer, 100, 0);
        printf("assigned: %s\n", buffer);
        //zmq_send(replier, "", 0, 0);
    }

    zmq_close(puller);
    zmq_ctx_destroy(context);

    return 0;
}

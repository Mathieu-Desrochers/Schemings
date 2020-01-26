#include <zmq.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <assert.h>

int main(int argc, char **argv)
{
    void* context = zmq_ctx_new();
    void* pusher = zmq_socket(context, ZMQ_PUSH);
    int rc = zmq_connect(pusher, "ipc://jobs-queue.ipc");
    assert(rc == 0);

    while (1)
    {
        char buffer[100];
        printf("job from fastcgi %s\n", argv[1]);
        sprintf(buffer, "job from fastcgi %s\n", argv[1]);
        zmq_send(pusher, buffer, strlen(buffer), 0);
        sleep(5);
    }

    zmq_close(pusher);
    zmq_ctx_destroy(context);

    return 0;
}

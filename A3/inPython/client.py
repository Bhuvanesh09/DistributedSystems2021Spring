import Pyro5.api
import ipdb
import os

requestObject = Pyro5.api.Proxy("PYRO:mstServer@localhost:6969")

requestObject.notifyConnection(os.getpid())
while(True):
    try:
        command = input()
    except EOFError:
        exit(0)

    commandAr = command.split()
    
    if commandAr[0] == "add_graph":
        _, identifier, N = commandAr
        N = int(N)
        requestObject.addGraph(identifier, N)
    elif commandAr[0] == "add_edge":
        _, identifier, u, v, wt = commandAr
        u, v, wt = int(u), int(v), int(wt)
        requestObject.addEdge(identifier, u, v, wt)
    elif commandAr[0] == "get_mst":
        _, identifier = commandAr
        print(requestObject.getMst(identifier))
    else:
        print("Unknown Command :(")
        ipdb.set_trace()




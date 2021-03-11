import Pyro5.api 
import copy
import numpy
import ipdb

class Graph:
    def __init__(self, N):
        self.numberNodes = N
        self.edges = []

    def addEdge(self, arg):
        u, v, wt = arg
        if u > self.numberNodes or  v > self.numberNodes :
            return 0

        self.edges.append((u, v, wt))

        return 1

    def printGraph(self):
        print(self.edges)

    def find(self, parent, i):
        if parent[i] == i:
            return i
        return self.find(parent, parent[i])

    def union(self, parent, rank, n1, n2):
            r1 = self.find(parent, n1)
            r2 = self.find(parent, n2)

            if rank[r1] < rank[r2]:
                parent[r1] = r2
            elif rank[r1] > rank[r1]:
                parent[r2] = r1
            else:
                parent[r1] = r2
                rank[r2] += 1

    def getMst(self):
        
        #sorting the edges by weight:
        tempEdges = copy.deepcopy(self.edges)
        tempEdges.sort(key = lambda x: x[2])

        curNodes = set()
        curEdges = []

        parent = [i for i in range(0, self.numberNodes+1)]
        rank = [0] * (self.numberNodes+1)

        while len(curEdges) < self.numberNodes-1 \
                and len(tempEdges) != 0:
            u, v, wt = tempEdges[0]
            tempEdges = tempEdges[1:]
            
            r1 = self.find(parent, u)
            r2 = self.find(parent, v)

            if r1 != r2:

                curNodes.add(u)
                curNodes.add(v)

                curEdges.append((u, v, wt))
                self.union(parent, rank, r1, r2)

        if len(curEdges) != self.numberNodes-1:
            return -1
        else:
            totalWt = 0
            for edge in curEdges:
                u, v, wt = edge
                totalWt += wt

            return totalWt
# ipdb.set_trace()

@Pyro5.api.expose 
class RequestMaker:
    def __init__(self):
        self.graphDict = {}

    def addGraph(self, identifier, N):
        if identifier in self.graphDict.keys():
            return 0
        self.graphDict[identifier] = Graph(N)
        return 1

    def addEdge(self, identifier, u, v, wt):
        if identifier not in self.graphDict.keys():
            return 0

        self.graphDict[identifier].addEdge((u, v, wt))

    def getMst(self, identifier):
        if identifier not in self.graphDict.keys():
            return -1

        return self.graphDict[identifier].getMst() 

    def decoyFunc(self, string):
        return "Server has got {} as the string\n".format(string)

    def notifyConnection(self, id):
        print("Client with pid {} has connected.".format(id))

daemon = Pyro5.server.Daemon(port=6969)

# nameserver = Pyro5.api.locate_ns()

RequestObject = RequestMaker()
uri = daemon.register(RequestObject, objectId="mstServer")
print(uri)
# nameserver.register("request_maker", uri)

print("The server is ready to accept requests from clients.")

daemon.requestLoop()


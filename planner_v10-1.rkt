#lang dssl2

# Final project: Trip Planner
let eight_principles = ["Know your rights.",
"Acknowledge your sources.",
"Protect your work.",
"Avoid suspicion.",
"Do your own work.",
"Never falsify a record or permit another person to do so.",
"Never fabricate data, citations, or experimental results.",
"Always tell the truth when discussing your work with your instructor."]

import cons
import sbox_hash
import 'project-lib/project-lib/binheap.rkt'


### Basic Types ###

#  - Latitudes and longitudes are numbers:
let Lat?  = num?
let Lon?  = num?

#  - Point-of-interest categories and names are strings:
let Cat?  = str?
let Name? = str?

### Raw Item Types ###

#  - Raw positions are 2-element vectors with a latitude and a longitude
let RawPos? = TupC[Lat?, Lon?]

#  - Raw road segments are 4-element vectors with the latitude and
#    longitude of their first endpoint, then the latitude and longitude
#    of their second endpoint
let RawSeg? = TupC[Lat?, Lon?, Lat?, Lon?]

#  - Raw points-of-interest are 4-element vectors with a latitude, a
#    longitude, a point-of-interest category, and a name
let RawPOI? = TupC[Lat?, Lon?, Cat?, Name?]

### Contract Helpers ###

# ListC[T] is a list of `T`s (linear time):
let ListC = Cons.ListC
# List of unspecified element type (constant time):
let List? = Cons.list?
let Vertex? = nat?

interface TRIP_PLANNER:

    # Returns the positions of all the points-of-interest that belong to
    # the given category.
    def locate_all(
            self,
            dst_cat:  Cat?           # point-of-interest category
        )   ->        ListC[RawPos?] # positions of the POIs

    # Returns the shortest route, if any, from the given source position
    # to the point-of-interest with the given name.
    def plan_route(
            self,
            src_lat:  Lat?,          # starting latitude
            src_lon:  Lon?,          # starting longitude
            dst_name: Name?          # name of goal
        )   ->        ListC[RawPos?] # path to goal

    # Finds no more than `n` points-of-interest of the given category
    # nearest to the source position.
    def find_nearby(
            self,
            src_lat:  Lat?,          # starting latitude
            src_lon:  Lon?,          # starting longitude
            dst_cat:  Cat?,          # point-of-interest category
            n:        nat?           # maximum number of results
        )   ->        ListC[RawPOI?] # list of nearby POIs

struct _building:
    let point
    let category

struct _location: # the location of a point, two road segments can form a road
    let pos1
    let pos2
    
struct _data: 
    let key
    let value

#graph structs
struct WEdge:
    let u
    let v
    let w
    
struct _Edge: 
    let neighbor 
    let w

struct dist_pairs:
    let vector_idx
    let distance
    
class AssociationList[K, V] ():

    let _head
    let _key_counter

    def __init__(self):
       self._head = None
       self._key_counter = 0
       

    # initilize empty associaiton list
    def __print__(self, print):
        print("#<object:AssociationList head=%p>", self._head)

    def len(self):        
        return self._key_counter
        
    def mem?(self, K):
        if self.len() == 0:
            return False
        let current = self._head
        while current != None: 
            if current.data.key == K:
                return True
            current = current.next
        
        return False 
  
        
    def get(self, K): 
        let current = self._head
        while current != None: 
            if current.data.key == K:
                return current.data.value
            current = current.next
            
        return None
        
    def get_counter(self, K): 
        let current = self._head
        let counter = 0
        while current != None: 
            if current.data.key == K:
                return counter
            current = current.next
            counter = counter + 1
            
        return None    
   
     # Modifies the dictionary to associate the given key and value. If the
    # key already exists, its value is replaced.
    def put(self, K, V):
        # if self.length == 0 add it 
        # if length is != 0, use the mem function to check if key is present, and update with new value
        # Insert at start
        if self.mem?(K) == True: 
            self._key_counter = self._key_counter - 1
            self.del(K)    
                   
        let value = _data(K , V)
        self._head = cons(value, self._head)
        self._key_counter = self._key_counter + 1    
        return
                               
        
    def del(self, K):
        let current = self._head
        if self.mem?(K) == False: 
            return
            
        if current.data.key == K:
            self._head = current.next
            self._key_counter = self._key_counter - 1
            return
        
        while current != None: 
            if current.next != None and current.next.data.key == K:
                current.next = current.next.next
                self._key_counter = self._key_counter - 1
            current = current.next
        return
        
    def get_names(self, wanted_category):
        let current = self._head
        let result_vector = None
        while current != None: 
            if current.data.value.category == wanted_category:
                result_vector = cons(current.data.key, result_vector)
            current = current.next
        if result_vector == None:
            return None
        return result_vector 
        
        
    def get_point_vector(self, wanted_category, counter):
        let current = self._head
        let result_vector = None
        let temp_hash = HashTable(counter, make_sbox_hash())
        while current != None: 
            if current.data.value.category == wanted_category:
                
                let new_data_1 = current.data.value.point.pos1 #struct reformatting
                let new_data_2 = current.data.value.point.pos2
                let new_value = [new_data_1, new_data_2]
                
                if temp_hash.get(new_value) != True: 
                    result_vector = cons(new_value, result_vector)
                    temp_hash.put(new_value, True)
                    
            current = current.next
        if result_vector == None:
            return None
        return result_vector 
        
    def get_name_from_loc(self, locationx, locationy):
        let current = self._head
        let result = None
        while current != None: 
            if current.data.value.point.pos1 == locationx:
                if current.data.value.point.pos2 == locationy:
                    result = cons(current.data.key, result) 
            current = current.next
     #       println("current %p", current)
     #   println("result %p", result)
     #   println("")
        return result
        
#Check edge case of end later
class HashTable[K, V] ():
    let _hash
    let _size
    let _hdata

    def __init__(self, nbuckets: nat?, hash: FunC[AnyC, nat?]):
        self._hash = hash
        self._size = 0
        self._hdata = [ None; max(1, nbuckets)] 
        for n in range(nbuckets):
           self._hdata[n] = AssociationList()
                
    def len(self): 
        return self._size
        
    def mem?(self, key: K):
        let i = self._find_bucket(key)
        return self._hdata[i].mem?(key)
        # has number % num of buckets
        # call mem on the bucket index 
        
    def _find_bucket(self, key: K): 
        return self._hash(key) % self._hdata.len()
        
    def get(self, key: K): 
        if self.mem?(key) == True:  
            let indx = self._find_bucket(key)
            let data = self._hdata[indx]
            return data.get(key)  
        return None             
        
    def put(self, key: K, value: V):
        let bucket_index = self._find_bucket(key)
        let bucket = self._hdata[bucket_index]
        if not self.mem?(key):
            self._size = self._size + 1
        bucket.put(key,value)   
        
    def del(self, key: K):
        let bucket_index = self._find_bucket(key)
        let bucket = self._hdata[bucket_index]
        
        if self.mem?(key) == True: 
            self._size = self._size - 1
        bucket.del(key)

class WUGraph ():
    let size
    let head
    let tail
    let _hdata
    
    def __init__(self, size: nat?):
        self.size = 0
        self.head = None
        self.tail = None
        self._hdata = [ None; max(1, size)] 
        
    def len(self): 
        return self.size 
        
    def set_edge(self, u, v, w):
        if w == None: 
            self.remove_edge(u,v,w)
            
        if self.get_edge(u,v) != None:
            self.update_weight(u,v,w)
            
        else:
            #For u
            let element2 = _Edge(v,w)
            
            #For v
            let element = _Edge(u,w)
            
            self._hdata[u] = cons(element2, self._hdata[u]) 
            self._hdata[v] = cons(element, self._hdata[v]) 
            self.size = self.size + 1
            return
            
    def remove_edge(self, u, v, w):
        # for u 
        let current = self._hdata[u]
        if current.head == None:
            return error("empty list")
        let replacement_head = current.head.next
        
        while current != None: 
            if current.data.neighbor == v: #current.data.neighbor 
                self.size = self.size - 1
                self.head = replacement_head        
                return
            current = current.next 
            
       # for v 
        current = self._hdata[v]
        if current.head == None:
            return error("empty list")
        replacement_head = current.head.next
        
        while current != None: 
            if current.data.neighbor == u:
                self.size = self.size - 1
                self.head = replacement_head        
                return
            current = current.next 
        
    def update_weight(self, u , v, w): 
        # for u 
        let current = self._hdata[u]
        while current != None: 
            if current.data.neighbor == v:
                current.data.w = w
            current = current.next 
        # for v
        current = self._hdata[v]
        while current != None: 
            if current.data.neighbor == u:
                current.data.w = w
            current = current.next 
        
    def get_edge(self,u,v): 
        if self.len() == 0:
            return None
        let current = self._hdata[u]
        while current != None: 
            if current.data.neighbor == v:
               return current.data.w
            current = current.next
    
    def get_adjacent(self, v): 
        let result = None 
        let current = self._hdata[v]
        while current != None: 
            result = cons(current.data.neighbor, result) 
            current = current.next
        return result
        
        
    def get_all_edges(self): 
        
        let result = None
        if self.len() == 0:
            return None
        for i in range(self.len()):   
            let current = self._hdata[i]
            while current != None: 
               if i <= current.data.neighbor:
                   result = cons(WEdge(i, current.data.neighbor, current.data.w), result) 
                   current = current.next
        return result 
                          
class TripPlanner (TRIP_PLANNER):
    let POI_counter
    
    let data_road
    
    let a 
    let h
    let nodeID_to_POS                        # for the second function hash Positions with Vertices 
    let POS_find_vertex  
        
    let WUresult                                #weighted tree
           

    
    def __init__(self, raw_road_segments, raw_POI):
        self.POI_counter = 0
        
        
        self.a = AssociationList()
        self.h = HashTable(raw_road_segments.len(), make_sbox_hash())
        
        self.data_road = [None; max(1,raw_road_segments.len())] 
            
        for i in range(raw_POI.len()):
            self.POI_put(raw_POI[i][0], raw_POI[i][1], \
                         raw_POI[i][2], raw_POI[i][3])

        self.WUresult = WUGraph(2*raw_road_segments.len())
        self.POS_find_vertex = AssociationList()
        self.nodeID_to_POS = HashTable(raw_road_segments.len(), make_sbox_hash())
        
        self.road_put(raw_road_segments)
      #  print("POI %p", self.a)
        
            
    def road_put(self, raw_roads): #store dictionary inside array to index
        for i in range(raw_roads.len()):
            let coor = [raw_roads[i][0], raw_roads[i][1],raw_roads[i][2], raw_roads[i][3]]
            let P = _location(raw_roads[i][0], raw_roads[i][1])
            let P2 = _location(raw_roads[i][2], raw_roads[i][3])
            # not all vertex has POI's                                 
        #    println("self.nodeID_to_POS.len() %p", self.nodeID_to_POS.len())
        #    println("self.POS_find_vertex.len() %p", self.POS_find_vertex.len())

           # if self.POS_find_vertex.get(P) != None:     
            if self.POS_find_vertex.get(P) == None: #hash table here --> input of location output vertices    
                self.nodeID_to_POS.put(self.nodeID_to_POS.len(), P) 
                self.POS_find_vertex.put(P,self.POS_find_vertex.len()) 
              #  println("POS_find_vertex %p %p", P,self.POS_find_vertex.len())
           
            if self.POS_find_vertex.get(P2) == None:    
                self.POS_find_vertex.put(P2,self.POS_find_vertex.len())      
                self.nodeID_to_POS.put(self.nodeID_to_POS.len(), P2) 
               # println("POS_find_vertex %p %p",self.POS_find_vertex.len(), P)
                
        #    print("POS_Find_vertex %p", self.POS_find_vertex)    
         #   print("nodeID_to_POS %p", self.nodeID_to_POS)    

            self.add_weights(raw_roads, coor, i)      
                 
                
       # println("NodeID_to_POS: %p", self.nodeID_to_POS)
            # add coors to dictionary if it's not already there and count up, coords to vertex dictionary 
    def add_weights(self, raw_roads,coor, i):                    
            let vertex_num = self.POS_find_vertex.get(_location(raw_roads[i][0],raw_roads[i][1]))
            let vertex_num2 = self.POS_find_vertex.get(_location(raw_roads[i][2],raw_roads[i][3]))
                        
            let u = vertex_num
            let v = vertex_num2
           
              # weight of edges 
            let distancex = raw_roads[i][2] - raw_roads[i][0]
            let distancey = raw_roads[i][3] - raw_roads[i][1]
            let slope = ((distancex)** 2 + (distancey)** 2).sqrt()
                    
            self.WUresult.set_edge(u, v, slope)
                                                                                                                        
    def POI_put(self,P1,P2,C,N): # stack
        let a = self.a
        let category_name = C
        let P = _location(P1,P2)                                                       #change for being able to account for multiple POI's at one vector
        let value = _building(P,C) 
        
        if a.mem?(N) == True: 
           # let temp_store = a.get(N)
            a.del(N)        
            self.POI_counter = self.POI_counter - 1     
           # let new_value = cons(N, temp_store)                                
           # a.put()                   
       # println("Name: %p was inserted into POI", N)
        a.put(N, value)   
        self.POI_counter = self.POI_counter + 1 
               
       # print("POI_Put %p", a.get(N))
        #save another association list but based on category and naems
    
    def get_name_based_on_category(self, wanted_category):   
         if self.POI_counter == 0:
             return None
         let a = self.a
         # find the names of the buildings in that category inside an array
         a.get_point_vector(wanted_category, self.POS_find_vertex.len())
                  
         # find the points of those buildings by calling a.get and indexing the array
    
    def get_name_from_loc(self, src_lat, src_lon):
         if self.POI_counter == 0:
             return error("no POI's in catelogue")    
         let a = self.a     
         a.get_name_from_loc(src_lat, src_lon)          
         
         # second function        
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~          
        
    def locate_all(self, dst_cat):
        print('''
        ----
        Locate_all
        ----
        ''')
        return self.get_name_based_on_category(dst_cat)

    def plan_route(self, src_lat, src_lon, dst_name):
        print('''
        ----
        Plan_route
        ----
        ''')

        println("planning route...")
        let dijkstra_return = self.dijkstra(src_lat, src_lon)
        let place_to = self.a.get(dst_name)
        if place_to == None:
            return None 
        
        let root_idx = self.POS_find_vertex.get(place_to.point)

        let distance = dijkstra_return[0]
        if distance[root_idx] == +inf:  
            return None
        let pred = dijkstra_return[1]       
        let temp_store = self.nodeID_to_POS.get(root_idx)
        
        let result_list = cons([temp_store.pos1, temp_store.pos2], None)
        let current_idx = root_idx

        while pred[current_idx] != None:
            let value = pred[current_idx] 
            current_idx = value 
            value = self.nodeID_to_POS.get(value)
            value = [value.pos1, value.pos2]
            result_list = cons(value,result_list) 
            
        return result_list
        
    def dijkstra(self, src_lat, src_lon):    
        let dist = [+inf  ; max(1, self.POS_find_vertex.len())]
        let pred = [None; max(1, self.POS_find_vertex.len())]
        let size = self.POS_find_vertex.len()
            
        let temp_loc = _location(src_lat, src_lon)
        let _root = self.POS_find_vertex.get(temp_loc)
        dist[_root] = 0 
        
        let todo = BinHeap[nat?](self.POS_find_vertex.len()**2, λ x, y: dist[x] < dist[y])                                                         # binheap the num of road segments there are
        let done = [False; max(1, self.POS_find_vertex.len())]
        todo.insert(_root)
        while todo.len() != 0:
            let temp_root = todo.find_min() #find min and remove
            todo.remove_min()
            if done[temp_root] != True:
                done[temp_root] = True
                
                let all_neighbors = self.WUresult.get_adjacent(temp_root)
                while all_neighbors != None: 
                    let current = all_neighbors.data
                    let w = self.WUresult.get_edge(temp_root, current) # retrieving weights
                    if dist[temp_root] + w < dist[current]:
                        dist[current] = dist[temp_root] + w
                        pred[current] = temp_root 
                        todo.insert(current) # self.g.get() returns the vertex of the location that we want 
                    all_neighbors = all_neighbors.next                  
        return [dist, pred]
        
    def find_nearby(self, src_lat, src_lon, dst_cat, n):
        let result = None    
        let dijkstra_return = self.dijkstra(src_lat, src_lon)
        let dist = dijkstra_return[0]
        let counter = 0
      #  println("dist %p", dist)
        #node to POI   
        # find minimum n number of indices/vertex     
        
       # let index_wanted = self.find_nearby_helper(dist, dst_cat, n) 
        let length_of_vector = self.nodeID_to_POS.len()
        for i in range (dist.len()):
            if dist[i] == +inf:
                length_of_vector = self.WUresult.len()
              #  println ("this was triggered")
        
        let vector_dist_pairs = [None; length_of_vector]
        for i in range (vector_dist_pairs.len()):
        #    println("in progress: %p", dist[i])
            if dist[i] != +inf:
                vector_dist_pairs[i] = dist_pairs(i, dist[i])
        heap_sort(vector_dist_pairs, self.lt)   
        
      #  println("vector_dist_pairs %p", vector_dist_pairs)
       # print("vector_dist_pair %p", vector_dist_pairs)
        for i in range (vector_dist_pairs.len()):                
            if vector_dist_pairs[i] != None: 
                let prename =  self.nodeID_to_POS.get(vector_dist_pairs[i].vector_idx)
                let name = self.get_name_from_loc(prename.pos1, prename.pos2)
               # println("name %p", name)
                if name != None:  
                  #  println("name.data %p", name.data)
                    while name != None:
                        let POI = self.a.get(name.data)
                        if POI.category == dst_cat and counter < n:
                            counter = counter + 1
                            result = cons([POI.point.pos1, POI.point.pos2, POI.category, name.data], result)   
                        #println("ok...this worked")
                        name = name.next
        return result
        
    def lt(self, dp1, dp2):  
      #  println("dp1: %p, dp2: %p", dp1, dp2)
        if dp1.distance > dp2.distance:
            return False
        if dp1.distance < dp2.distance:
            return True
        if dp1.distance == dp2.distance:
            return True
      
def my_first_example():
    return TripPlanner([[0,0, 0,1], [0,0, 1,0]],
                       [[0,0, "bar", "The Empty Bottle"],
                        [0,1, "food", "Pierogi"]]) 
                        
test 'My first locate_all test':
    assert my_first_example().locate_all("food") == cons([0,1], None)
test 'My first plan_route test':
    assert my_first_example().plan_route(0, 0, "Pierogi") == cons([0,0], cons([0,1], None))
test 'My first find_nearby test':
    assert my_first_example().find_nearby(0, 0, "food", 1) == cons([0,1, "food", "Pierogi"], None)
test "error #1":
    let tp = TripPlanner(
          [[0, 0, 1, 0]],
          [[1, 0, 'bank', 'Union']])
    let result = tp.locate_all('food')
    assert Cons.to_vec(result) == []
test "error #2": 
    let tp = TripPlanner(
      [[0, 0, 1.5, 0],
       [1.5, 0, 2.5, 0],
       [2.5, 0, 3, 0]],
      [[1.5, 0, 'bank', 'Union'],
       [3, 0, 'barber', 'Tony']])
    let result = tp.plan_route(0, 0, 'Judy')
    assert Cons.to_vec(result)  == []
test "error #3 Destination isn't reachable": 
    let tp = TripPlanner(
      [[0, 0, 1.5, 0],
       [1.5, 0, 2.5, 0],
       [2.5, 0, 3, 0],
       [4, 0, 5, 0]],
      [[1.5, 0, 'bank', 'Union'],
       [3, 0, 'barber', 'Tony'],
       [5, 0, 'barber', 'Judy']])
    let result = tp.plan_route(0, 0, 'Judy')
    assert Cons.to_vec(result)  == []


test "MST is not SSSP (nearby)": 
    let tp = TripPlanner(
      [[-1.1, -1.1, 0, 0],
       [0, 0, 3, 0],
       [3, 0, 3, 3],
       [3, 3, 3, 4],
       [0, 0, 3, 4]],
      [[0, 0, 'food', 'Sandwiches'],
       [3, 0, 'bank', 'Union'],
       [3, 3, 'barber', 'Judy'],
       [3, 4, 'barber', 'Tony']])
    let result = tp.find_nearby(-1.1, -1.1, 'barber', 1)
    assert Cons.to_vec(result) \
      == [[3, 4, 'barber', 'Tony']]



test "One barber nearby":
    let tp = TripPlanner(
      [[0, 0, 1.5, 0],
       [1.5, 0, 2.5, 0],
       [2.5, 0, 3, 0]],
      [[1.5, 0, 'bank', 'Union'],
       [3, 0, 'barber', 'Tony']])
    let result = tp.find_nearby(0, 0, 'barber', 1)
    assert Cons.to_vec(result) == [[3, 0, 'barber', 'Tony']]

test "find bank from barber":
    let tp = TripPlanner(
      [[0, 0, 1.5, 0],
       [1.5, 0, 2.5, 0],
       [2.5, 0, 3, 0]],
      [[1.5, 0, 'bank', 'Union'],
       [3, 0, 'barber', 'Tony']])
    let result = tp.find_nearby(3, 0, 'bank', 1)
    assert Cons.to_vec(result) == [[1.5, 0, 'bank', 'Union']]

test "Failed test: 3 relevant POIs; farther 2 at same location; limit 2":
    let tp = TripPlanner(
      [[0, 0, 1.5, 0],
       [1.5, 0, 2.5, 0],
       [2.5, 0, 3, 0],
       [4, 0, 5, 0],
       [3, 0, 4, 0]],
      [[1.5, 0, 'bank', 'Union'],
       [3, 0, 'barber', 'Tony'],
       [5, 0, 'barber', 'Judy'],
       [5, 0, 'barber', 'Lily']])
    let result = tp.find_nearby(0, 0, 'barber', 2)
    assert Cons.to_vec(result) == [[5, 0, 'barber', 'Judy'], [3, 0, 'barber', 'Tony']] # just needs to be one of     

test "Failed test: 2 relevant equidistant POIs; limit 1":
    let tp = TripPlanner(
      [[-1, -1, 0, 0],
       [0, 0, 3.5, 0],
       [0, 0, 0, 3.5],
       [3.5, 0, 0, 3.5]],
      [[-1, -1, 'food', 'Jollibee'],
       [0, 0, 'bank', 'Union'],
       [3.5, 0, 'barber', 'Tony'],
       [0, 3.5, 'barber', 'Judy']])
    let result = tp.find_nearby(-1, -1, 'barber', 1)
    assert Cons.to_vec(result) == \  #this one can be either or
                   [[0, 3.5, 'barber', 'Judy']]    

                   
                   
test "Failed test: 3 relevant POIs; farther 2 equidistant; limit 2":
    let tp = TripPlanner(
      [[0, 0, 1.5, 0],
       [1.5, 0, 2.5, 0],
       [2.5, 0, 3, 0],
       [4, 0, 5, 0],
       [3, 0, 4, 0]],
      [[1.5, 0, 'bank', 'Union'],
       [0, 0, 'barber', 'Lily'],
       [3, 0, 'barber', 'Tony'],
       [5, 0, 'barber', 'Judy']])
    let result = tp.find_nearby(2.5, 0, 'barber', 2)
    assert Cons.to_vec(result) == [[0, 0, 'barber', 'Lily'], [3, 0, 'barber', 'Tony']]  # just needs to be one of
                   

package main

import (
	"encoding/json"
	"flag"
	"html/template"
	"log"
	"math/rand"
	"net"
	"net/http"
	"os"
	"os/exec"
	"runtime"
	"sort"
	"strconv"

	mgo "gopkg.in/mgo.v2"
	"gopkg.in/mgo.v2/bson"
)

const (
	connectionString = "tfb-database"
	worldRowCount    = 10000
	fortuneHTML      = `<!DOCTYPE html>
<html>
<head>
<title>Fortunes</title>
</head>
<body>
<table>
<tr>
<th>id</th>
<th>message</th>
</tr>
{{range .}}
<tr>
<td>{{.Id}}</td>
<td>{{.Message}}</td>
</tr>
{{end}}
</table>
</body>
</html>`
)

var (
	tmpl = template.Must(template.New("fortune.html").Parse(fortuneHTML))

	database *mgo.Database
	fortunes *mgo.Collection
	worlds   *mgo.Collection
)

type Message struct {
	Message string `json:"message"`
}

type World struct {
	Id           uint16 `bson:"_id" json:"id"`
	RandomNumber uint16 `bson:"randomNumber" json:"randomNumber"`
}

type Fortune struct {
	Id      uint16 `bson:"_id" json:"id"`
	Message string `bson:"message" json:"message"`
}

type Fortunes []Fortune

func (s Fortunes) Len() int {
	return len(s)
}

func (s Fortunes) Swap(i, j int) {
	s[i], s[j] = s[j], s[i]
}

type ByMessage struct{ Fortunes }

func (s ByMessage) Less(i, j int) bool {
	return s.Fortunes[i].Message < s.Fortunes[j].Message
}

var prefork = flag.Bool("prefork", false, "use prefork")
var child = flag.Bool("child", false, "is child proc")

func main() {
	var listener net.Listener
	flag.Parse()
	if !*prefork {
		runtime.GOMAXPROCS(runtime.NumCPU())
	} else {
		listener = doPrefork()
	}

	session, err := mgo.Dial(connectionString)
	if err != nil {
		log.Fatalf("Error opening database: %v", err)
	}
	defer session.Close()
	database = session.DB("hello_world")
	worlds = database.C("world")
	fortunes = database.C("fortune")
	http.HandleFunc("/db", dbHandler)
	http.HandleFunc("/fortune", fortuneHandler)
	http.HandleFunc("/queries", queriesHandler)
	http.HandleFunc("/update", updateHandler)

	if !*prefork {
		http.ListenAndServe(":8080", nil)
	} else {
		http.Serve(listener, nil)
	}
}

func doPrefork() net.Listener {
	var listener net.Listener
	if !*child {
		addr, err := net.ResolveTCPAddr("tcp", ":8080")
		if err != nil {
			log.Fatal(err)
		}
		tcplistener, err := net.ListenTCP("tcp", addr)
		if err != nil {
			log.Fatal(err)
		}
		fl, err := tcplistener.File()
		if err != nil {
			log.Fatal(err)
		}
		children := make([]*exec.Cmd, runtime.NumCPU()/2)
		for i := range children {
			children[i] = exec.Command(os.Args[0], "-prefork", "-child")
			children[i].Stdout = os.Stdout
			children[i].Stderr = os.Stderr
			children[i].ExtraFiles = []*os.File{fl}
			err = children[i].Start()
			if err != nil {
				log.Fatal(err)
			}
		}
		for _, ch := range children {
			if err := ch.Wait(); err != nil {
				log.Print(err)
			}
		}
		os.Exit(0)
	} else {
		var err error
		listener, err = net.FileListener(os.NewFile(3, ""))
		if err != nil {
			log.Fatal(err)
		}
	}
	return listener
}

// Helper for random numbers
func getRandomNumber() uint16 {
	return uint16(rand.Intn(worldRowCount) + 1)
}

// Test 2: Single database query
func dbHandler(w http.ResponseWriter, r *http.Request) {
	var world World
	query := bson.M{"_id": getRandomNumber()}
	if err := worlds.Find(query).One(&world); err != nil {
		log.Fatalf("Error finding world with id: %s", err.Error())
	}
	w.Header().Set("Content-Type", "application/json")
	w.Header().Set("Server", "Go")
	json.NewEncoder(w).Encode(&world)
}

// Helper for getting the "queries" parameter
func getQueriesParam(r *http.Request) int {
	n := 1
	if nStr := r.URL.Query().Get("queries"); len(nStr) > 0 {
		n, _ = strconv.Atoi(nStr)
	}

	if n < 1 {
		n = 1
	} else if n > 500 {
		n = 500
	}

	return n
}

// Test 3: Multiple database queries
func queriesHandler(w http.ResponseWriter, r *http.Request) {
	n := getQueriesParam(r)
	world := make([]World, n)

	for i := 0; i < n; i++ {
		query := bson.M{"_id": getRandomNumber()}
		if err := worlds.Find(query).One(&world[i]); err != nil {
			log.Fatalf("Error finding world with id: %s", err.Error())
		}
	}

	w.Header().Set("Content-Type", "application/json")
	w.Header().Set("Server", "Go")
	json.NewEncoder(w).Encode(world)
}

// Test 4: Fortunes
func fortuneHandler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "text/html; charset=utf-8")
	w.Header().Set("Server", "Go")
	f := make(Fortunes, 16)
	if err := fortunes.Find(nil).All(&f); err == nil {
		f = append(f, Fortune{Message: "Additional fortune added at request time."})
		sort.Sort(ByMessage{f})
		if err := tmpl.Execute(w, f); err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
		}
	} else {
		http.Error(w, err.Error(), http.StatusInternalServerError)
	}
}

// Test 5: Database updates
func updateHandler(w http.ResponseWriter, r *http.Request) {
	n := getQueriesParam(r)
	world := make([]World, n)

	for i := 0; i < n; i++ {
		query := bson.M{"_id": getRandomNumber()}
		if err := worlds.Find(query).One(&world[i]); err != nil {
			log.Fatalf("Error finding world with id: %s", err.Error())
		}
		world[i].RandomNumber = getRandomNumber()
		update := bson.M{"$set": bson.M{"randomNumber": world[i].RandomNumber}}
		if err := worlds.Update(query, update); err != nil {
			log.Fatalf("Error updating world with id: %s", err.Error())
		}
	}

	w.Header().Set("Content-Type", "application/json")
	w.Header().Set("Server", "Go")
	json.NewEncoder(w).Encode(&world)
}

package main

import (
	"flag"
	"log"

	"github.com/go-www/silverlining"
)

var BindAddr string
var prefork *bool

func init() {
	flag.StringVar(&BindAddr, "bind", ":8080", "set bind host")
	prefork = flag.Bool("prefork", false, "use prefork")
	flag.Parse()
}

func main() {
	log.Printf("Listening on http://localhost%s", BindAddr)

	Handler := func(r *silverlining.Context) {
		switch string(r.Path()) {
		case "/plaintext":
			r.ResponseHeaders().Set("Content-Type", "text/plain")
			r.WriteFullBodyString(200, "Hello, World!")
		case "/json":
			type Message struct {
				Message string `json:"message"`
			}
			msg := Message{Message: "Hello, World!"}
			r.WriteJSON(200, msg)
		default:
			r.WriteFullBody(404, nil)
		}
	}

	var err error
	if *prefork {
		var id int
		id, err = silverlining.PreforkChildID()
		if err != nil {
			log.Fatalln(err)
		}

		if id == 0 {
			log.Println("Starting prefork leader process")
		} else {
			log.Printf("Starting prefork replica process %d", id)
		}
		err = silverlining.ListenAndServePrefork(BindAddr, Handler)
	} else {
		err = silverlining.ListenAndServe(BindAddr, Handler)
	}
	if err != nil {
		log.Fatalln(err)
	}
}

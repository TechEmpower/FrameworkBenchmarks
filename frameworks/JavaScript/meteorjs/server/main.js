import { Meteor } from 'meteor/meteor';
import { EJSON } from 'meteor/ejson'
import { WebApp } from 'meteor/webapp';

Meteor.startup(() => {

  WebApp.connectHandlers.use('/plaintext', (req, res) => {
    res.setHeader('Server', 'Meteor');
    res.setHeader('Content-Type', 'text/plain');
    res.end("Hello, World!");
  });

  WebApp.connectHandlers.use('/json', (req, res) => {
    res.setHeader('Server', 'Meteor');
    res.setHeader('Content-Type', 'application/json');
    res.end(EJSON.stringify({message: "Hello, World!"}));
  });
});

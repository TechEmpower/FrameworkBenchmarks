const db = process.env.DATABASE;

const { App } = require('@ionited/mesh');

const addHeaders = (res, contentType) => res.header('Content-Type', contentType).header('Server', 'Mesh');

const escapeHTMLRules = {
  "&": "&#38;",
  "<": "&#60;",
  ">": "&#62;",
  '"': "&#34;",
  "'": "&#39;",
  "/": "&#47;",
};

const unsafeHTMLMatcher = /[&<>"'\/]/g;

const escape = text => {
  if (unsafeHTMLMatcher.test(text) === false) return text;
  return text.replace(unsafeHTMLMatcher,  m => escapeHTMLRules[m] || m);
}

const random = () => Math.floor(Math.random() * 1e4) + 1;

const app = new App();

app

.get('/json', (_, res) => {
  addHeaders(res, 'application/json');

  res.json({ message: 'Hello, World!' });
})

.get('/plaintext', (_, res) => {
  addHeaders(res, 'text/plain');

  res.send('Hello, World!');
});

if (db) {
  const DRIVER = require(`./drivers/${db}`);

  app
  
  .get('/db', async (_, res) => {
    addHeaders(res, 'application/json');

    res.json(await DRIVER.find(random()));
  })
  
  .get('/queries', async (req, res) => {
    const { queries } = req.query();

    const count = Math.min(parseInt(queries) || 1, 500);

    const arr = [];

    for (let i = 0; i < count; i++) arr.push(await DRIVER.find(random()));

    addHeaders(res, 'application/json');

    res.json(arr);
  })
  
  .get('/fortunes', async (_, res) => {
    const items = [{
      id: 0,
      message: 'Additional fortune added at request time.'
    }, ...await DRIVER.fortunes()].sort((a, b) => a.message.localeCompare(b.message));

    let html = '<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>';

    for (let i = 0; i < items.length; i++) html += `<tr><td>${items[i].id}</td><td>${escape(items[i].message)}</td></tr>`;

    html += '</table></body></html>';

    addHeaders(res, 'text/html; charset=utf-8');

    res.send(html);
  })
  
  .get('/updates', async (req, res) => {
    const { queries } = req.query();

    const count = Math.min(parseInt(queries) || 1, 500);

    const arr = [];

    for (let i = 0; i < count; i++) arr.push(await DRIVER.find(random()));

    for (let i = 0; i < count; i++) {
      arr[i].randomNumber = random();
      
      await DRIVER.update(arr[i]);
    }

    addHeaders(res, 'application/json');

    res.json(arr);
  });
}

app.listen(8080);

module Web.View.FrameworkBenchmarks.Fortune where
import Web.View.Prelude

data FortuneView = FortuneView { fortunes :: [Fortune] }

instance View FortuneView ViewContext where
    beforeRender (context, view) = (context { layout = \v -> v }, view)
    html FortuneView { .. } = preEscapedToHtml ("<!DOCTYPE html>" :: Text) <> [hsx|
        <html>
            <head><title>Fortunes</title></head>
            <body>
                <table>
                    <tr>
                        <th>id</th>
                        <th>message</th>
                    </tr>
                    {forEach fortunes renderFortunes}
                </table>
            </body>
        </html>
    |]


renderFortunes fortune = [hsx|
    <tr>
        <td>{get #id fortune}</td>
        <td>{get #message fortune}</td>
    </tr>
|]

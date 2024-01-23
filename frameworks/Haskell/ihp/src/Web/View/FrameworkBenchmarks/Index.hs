module Web.View.FrameworkBenchmark.Index where
import Web.View.Prelude

data FortuneView = FortuneView { fortunes :: [Fortune] }

instance View FortuneView where
    html FortuneView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
            </ol>
        </nav>
        
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>FrameworkBenchmark</th>
                        <th></th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
            </table>
        </div>
    |]


renderFrameworkBenchmark frameworkBenchmark = [hsx|
    <tr>
        <td>{frameworkBenchmark}</td>
    </tr>
|]

<!DOCTYPE html>
<html>
	<head>
		<title>Error</title>
		<style>
			html{
				width:100%;
				min-height:100%;
				font-family:'Verdana';
				font-size:14px;
			}
			body{

				min-height:100%;
				background: #a90329; /* Old browsers */
				background: -moz-radial-gradient(center, ellipse cover, #a90329 0%, #6d0019 100%); /* FF3.6+ */
				background: -webkit-radial-gradient(center, ellipse cover, #a90329 0%,#6d0019 100%); /* Chrome10+,Safari5.1+ */
			}
			#content{
				width:1000px;
				margin:auto;
				padding:10px 0px;
				background:#eee;
			}
			.file{
				font-weight:bold;
			}
			.block{
				border-bottom:1px solid #000;
				margin:10px;
			}
			.code{
				
				padding:10px;
			}
			.highlight{
				background:#efecd0;
			}
			#exception{
				font-size:25px;
				font-weight:bold;
				padding:10px;
			}
			#debug{
				border-bottom: 1px solid black;
				margin: 10px;
			}
			#log{
				font-size:15px;
				font-weight:bold;
				padding:5px;
			}
			.log{
				padding:10px;
				border-bottom: 1px solid black;
			}
			.log.odd{
				
			}
			pre{
				margin:0px;
			}
			.thick{
				border-width:2px;
			}
		</style>
	</head>
	<body>
		<?php 
			$rawblocks=array_merge(array(array(
				'file'=>$exception->getFile(),
				'line'=>$exception->getLine()
			)), $exception->getTrace());
			$blocks = array();
			foreach($rawblocks as $block){
				if(!isset($block['file']))
					continue;
				//avoid duplicates
				if(count($blocks)>0){
					$last=$blocks[count($blocks)-1];
					if($block['file']==$last['file'] && $block['line']==$last['line'])
						continue;
				}
				$blocks[]=$block;
			}
			
			
		?>
		<div id="content">
			<div id="exception"><?php echo str_replace("\n",'<br/>',$exception->getMessage()); ?></div>
			<div id="blocks">
				<?php foreach($blocks as $bkey=>$block):	?>
					<div class="block <?php echo (!empty($log)&&$bkey==0)?'thick':''; ?>">
						<div class="file"><?php echo $block['file'];?></div>
						<div class="code">
							<?php 
								$line=$block['line']-1;
								$code = explode("\n", file_get_contents($block['file']));
								$start = $line - 3;
								if ($start < 0) $start = 0;
								$end = $line + 3;
								if($end>=count($code)) $end=count($code)-1;
								$code=array_slice($code,$start,$end-$start,true);
							?>
							
							<?php foreach($code as $n=>$text):?>
							<pre class="line <?php echo $n==$line?'highlight':''; ?>"><?php echo ($n+1).'    '.htmlspecialchars($text); ?></pre>
							<?php endforeach;?>
						</div>
					</div>
					<?php if($bkey==0&&!empty($log)):?>
						<div id="debug">
							<div id="log">Logged values:</div>
							<?php foreach($log as $key=>$val):?>
								<div class="log <?php echo $key%2?'odd':''; ?>">
									<pre><?php var_export($val);?></pre>
								</div>
							<?php endforeach;?>
						</div>
						<div id="log">Call stack:</div>
					<?php endif;?>
				<?php endforeach;?>
			</div>
		</div>
	</body>
</html>
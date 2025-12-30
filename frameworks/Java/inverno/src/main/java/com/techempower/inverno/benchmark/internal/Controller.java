package com.techempower.inverno.benchmark.internal;

import com.techempower.inverno.benchmark.model.Fortune;
import com.techempower.inverno.benchmark.model.Message;
import com.techempower.inverno.benchmark.model.World;
import com.techempower.inverno.benchmark.templates.FortunesTemplate;
import io.inverno.core.annotation.Bean;
import io.inverno.core.annotation.Bean.Visibility;
import io.inverno.core.annotation.Destroy;
import io.inverno.core.annotation.Init;
import io.inverno.mod.base.Charsets;
import io.inverno.mod.base.concurrent.Reactor;
import io.inverno.mod.base.concurrent.ReactorScope;
import io.inverno.mod.base.converter.ConverterException;
import io.inverno.mod.base.reflect.Types;
import io.inverno.mod.http.base.ExchangeContext;
import io.inverno.mod.http.base.HttpException;
import io.inverno.mod.http.base.Parameter;
import io.inverno.mod.http.base.Status;
import io.inverno.mod.http.server.ErrorExchange;
import io.inverno.mod.http.server.Exchange;
import io.inverno.mod.http.server.ServerController;
import io.inverno.mod.sql.UnsafeSqlOperations;
import io.netty.buffer.ByteBuf;
import io.netty.buffer.Unpooled;
import io.netty.channel.EventLoopGroup;
import io.netty.handler.codec.http.HttpHeaderNames;
import io.netty.handler.codec.http.HttpHeaderValues;
import io.netty.util.AsciiString;
import java.lang.reflect.Type;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ThreadLocalRandom;
import java.util.concurrent.TimeUnit;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@Bean( visibility = Visibility.PRIVATE )
public class Controller implements ServerController<ExchangeContext, Exchange<ExchangeContext>, ErrorExchange<ExchangeContext>> {

	private static final String PATH_PLAINTEXT = "/plaintext";
	private static final String PATH_JSON = "/json";
	private static final String PATH_DB = "/db";
	private static final String PATH_QUERIES = "/queries";
	private static final String PATH_UPDATES = "/updates";
	private static final String PATH_FORTUNES = "/fortunes";
	
	public static final String DB_SELECT_WORLD = "SELECT id, randomnumber from WORLD where id = $1";

	private static final CharSequence STATIC_SERVER = AsciiString.cached("inverno");

	private static final Type LIST_WORLD_TYPE = Types.type(List.class).type(World.class).and().build();

	private final Reactor reactor;
	private final ReactorScope<JsonSerializer> jsonSerializer;
	private final ReactorScope<Mono<DbRepository>> dbRepository;
	
	private EventLoopGroup dateEventLoopGroup;
	
	private CharSequence date;
	
	public Controller(Reactor reactor, 
			ReactorScope<JsonSerializer> jsonSerializer,
			ReactorScope<Mono<DbRepository>> dbRepository
		) {
		this.reactor = reactor;
		this.jsonSerializer = jsonSerializer;
		this.dbRepository = dbRepository;
	}
	
	@Init
	public void init() {
		this.dateEventLoopGroup = this.reactor.createIoEventLoopGroup(1);
		this.dateEventLoopGroup.scheduleAtFixedRate(() -> this.date = new AsciiString(DateTimeFormatter.RFC_1123_DATE_TIME.format(ZonedDateTime.now())), 0, 1000, TimeUnit.MILLISECONDS);
	}
	
	@Destroy
	public void destroy() {
		this.dateEventLoopGroup.shutdownGracefully();
	}
	
	@Override
	public void handle(Exchange<ExchangeContext> exchange) throws HttpException {
		switch(exchange.request().getPath()) {
			case PATH_PLAINTEXT: {
				this.handle_plaintext(exchange);
				break;
			}
			case PATH_JSON: {
				this.handle_json(exchange);
				break;
			}
			case PATH_DB: {
				this.handle_db(exchange);
				break;
			}
			case PATH_FORTUNES: {
				this.handle_fortunes(exchange);
				break;
			}
			default: {
				switch(exchange.request().getPathAbsolute()) {
					case PATH_QUERIES: {
						this.handle_queries(exchange);
						break;
					}
					case PATH_UPDATES: {
						this.handle_updates(exchange);
						break;
					}
					default: {
						exchange.response()
							.headers(h -> h.status(Status.NOT_FOUND))
							.body()
								.empty();
					}
				}
			}
		}
	}

	private static final byte[] STATIC_PLAINTEXT = "Hello, World!".getBytes(Charsets.UTF_8);
	private static final int STATIC_PLAINTEXT_LEN = STATIC_PLAINTEXT.length;

	private static final ByteBuf STATIC_PLAINTEXT_BYTEBUF;
	static {
		ByteBuf tmpBuf = Unpooled.buffer(STATIC_PLAINTEXT_LEN);
		tmpBuf.writeBytes(STATIC_PLAINTEXT);
		STATIC_PLAINTEXT_BYTEBUF = Unpooled.unreleasableBuffer(tmpBuf);
	}
	
	private static final CharSequence STATIC_PLAINTEXT_LEN_VALUE = AsciiString.cached(String.valueOf(STATIC_PLAINTEXT_LEN));

	private static final Mono<ByteBuf> PLAIN_TEXT_MONO = Mono.fromSupplier(STATIC_PLAINTEXT_BYTEBUF::duplicate);
	
	public void handle_plaintext(Exchange<ExchangeContext> exchange) throws HttpException {
		exchange.response()
			.headers(h -> h
				.add(HttpHeaderNames.SERVER, STATIC_SERVER)
				.add(HttpHeaderNames.DATE, date)
				.add(HttpHeaderNames.CONTENT_LENGTH, STATIC_PLAINTEXT_LEN_VALUE)
				.add(HttpHeaderNames.CONTENT_TYPE, HttpHeaderValues.TEXT_PLAIN)
			)
			.body()
				.raw()
					.stream(PLAIN_TEXT_MONO);
	}
	
	public void handle_json(Exchange<ExchangeContext> exchange) throws HttpException {
		exchange.response()
			.headers(h -> h
				.add(HttpHeaderNames.SERVER, STATIC_SERVER)
				.add(HttpHeaderNames.DATE, this.date)
				.add(HttpHeaderNames.CONTENT_TYPE, HttpHeaderValues.APPLICATION_JSON)
			)
			.body()
			.raw()
			.value(this.jsonSerializer.get().serialize(new Message("Hello, World!"), Message.class));
	}

	private static int randomWorldId() {
		return 1 + ThreadLocalRandom.current().nextInt(10000);
	}
	
	public void handle_db(Exchange<ExchangeContext> exchange) throws HttpException {
		exchange.response()
			.headers(h -> h
				.add(HttpHeaderNames.SERVER, STATIC_SERVER)
				.add(HttpHeaderNames.DATE, this.date)
				.add(HttpHeaderNames.CONTENT_TYPE, HttpHeaderValues.APPLICATION_JSON)
			)
			.body()
				.raw().stream(this.dbRepository.get()
					.flatMap(repository -> repository.getWorld(randomWorldId()))
					.map(world -> this.jsonSerializer.get().serialize(world, World.class))
				);
	}
	
	private static final String PARAMETER_QUERIES = "queries";

	private int extractQueriesParameter(Exchange<ExchangeContext> exchange) {
		try {
			return Math.min(500, Math.max(1, exchange.request().queryParameters().get(PARAMETER_QUERIES).map(Parameter::asInteger).orElse(1)));
		}
		catch (ConverterException e) {
			return 1;
		}
	}
	
	public void handle_queries(Exchange<ExchangeContext> exchange) throws HttpException {
		int queries = this.extractQueriesParameter(exchange);
		exchange.response()
			.headers(h -> h
				.add(HttpHeaderNames.SERVER, STATIC_SERVER)
				.add(HttpHeaderNames.DATE, this.date)
				.add(HttpHeaderNames.CONTENT_TYPE, HttpHeaderValues.APPLICATION_JSON)
			)
			.body()
				.raw().stream(this.dbRepository.get()
					.flatMapMany(repository -> ((UnsafeSqlOperations)repository.getSqlClient())
						.batchQueries(ops -> 
							Flux.range(0, queries)
								.map(ign -> ops.queryForObject(
									DB_SELECT_WORLD, 
									row -> new World(row.getInteger(0), row.getInteger(1)), 
									randomWorldId()
								))
						)
					)
					.collectList()
					.map(worlds -> this.jsonSerializer.get().serialize(worlds, LIST_WORLD_TYPE))
				);
	}
	
	public void handle_updates(Exchange<ExchangeContext> exchange) throws HttpException {
		int queries = this.extractQueriesParameter(exchange);
		exchange.response()
			.headers(h -> h
				.add(HttpHeaderNames.SERVER, STATIC_SERVER)
				.add(HttpHeaderNames.DATE, this.date)
				.add(HttpHeaderNames.CONTENT_TYPE, HttpHeaderValues.APPLICATION_JSON)
			)
			.body()
				.raw().stream(this.dbRepository.get()
					.flatMapMany(repository -> Flux.from(((UnsafeSqlOperations)repository.getSqlClient())
						.batchQueries(ops ->
							Flux.range(0, queries)
								.map(ign -> ops.queryForObject(
									DB_SELECT_WORLD,
									row -> new World(row.getInteger(0), randomWorldId()),
									randomWorldId()
								))
						))
						.collectSortedList()
						.delayUntil(repository::updateWorlds)
						.map(worlds -> this.jsonSerializer.get().serialize(worlds, LIST_WORLD_TYPE))
					)
				);
	}
	
	private static final CharSequence MEDIA_TEXT_HTML_UTF8 = AsciiString.cached("text/html; charset=utf-8");
	
	private static final FortunesTemplate.Renderer<CompletableFuture<ByteBuf>> FORTUNES_RENDERER = FortunesTemplate.bytebuf(Unpooled::buffer);
	
	public void handle_fortunes(Exchange<ExchangeContext> exchange) throws HttpException {
		exchange.response()
			.headers(h -> h
				.add(HttpHeaderNames.SERVER, STATIC_SERVER)
				.add(HttpHeaderNames.DATE, this.date)
				.add(HttpHeaderNames.CONTENT_TYPE, MEDIA_TEXT_HTML_UTF8)
			)
			.body()
				.raw().stream(this.dbRepository.get().flatMapMany(DbRepository::listFortunes)
					.collectList()
					.flatMap(fortunes -> {
						fortunes.add(new Fortune(0, "Additional fortune added at request time."));
						Collections.sort(fortunes);
						return Mono.fromFuture(() -> FORTUNES_RENDERER.render(fortunes));
					})
				);
	}
}

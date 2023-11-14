package com.techempower.inverno.benchmark.internal;

import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Collections;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ThreadLocalRandom;
import java.util.concurrent.TimeUnit;
import java.util.function.Supplier;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
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
import io.inverno.mod.http.base.HttpException;
import io.inverno.mod.http.base.InternalServerErrorException;
import io.inverno.mod.http.base.Parameter;
import io.inverno.mod.http.base.Status;
import io.inverno.mod.http.server.Exchange;
import io.inverno.mod.http.server.ExchangeContext;
import io.inverno.mod.http.server.RootExchangeHandler;
import io.inverno.mod.sql.SqlClient;
import io.inverno.mod.sql.UnsafeSqlOperations;
import io.netty.buffer.ByteBuf;
import io.netty.buffer.Unpooled;
import io.netty.channel.EventLoopGroup;
import io.netty.handler.codec.http.HttpHeaderNames;
import io.netty.handler.codec.http.HttpHeaderValues;
import io.netty.util.AsciiString;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@Bean( visibility = Visibility.PRIVATE )
public class Handler implements RootExchangeHandler<ExchangeContext, Exchange<ExchangeContext>> {

	private static final String PATH_PLAINTEXT = "/plaintext";
	private static final String PATH_JSON = "/json";
	private static final String PATH_DB = "/db";
	private static final String PATH_QUERIES = "/queries";
	private static final String PATH_UPDATES = "/updates";
	private static final String PATH_FORTUNES = "/fortunes";
	
	public static final String DB_SELECT_WORLD = "SELECT id, randomnumber from WORLD where id = $1";
	public static final String DB_UPDATE_WORLD = "UPDATE world SET randomnumber=$1 WHERE id=$2";
	public static final String DB_SELECT_FORTUNE = "SELECT id, message from FORTUNE";
	
	private static final CharSequence STATIC_SERVER = AsciiString.cached("inverno");

	private final Reactor reactor;
	private final ObjectMapper mapper;
	private final ReactorScope<Mono<SqlClient>> sqlClient;
	
	private EventLoopGroup dateEventLoopGroup;
	
	private CharSequence date;
	
	public Handler(Reactor reactor, 
			ObjectMapper mapper,
			ReactorScope<Mono<SqlClient>> sqlClient
		) {
		this.reactor = reactor;
		this.mapper = mapper;
		this.sqlClient = sqlClient;
	}
	
	@Init
	public void init() {
		this.dateEventLoopGroup = this.reactor.createIoEventLoopGroup(1);
		this.dateEventLoopGroup.scheduleAtFixedRate(() -> {
			this.date = new AsciiString(DateTimeFormatter.RFC_1123_DATE_TIME.format(ZonedDateTime.now()));
		}, 0, 1000, TimeUnit.MILLISECONDS);
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
		ByteBuf tmpBuf = Unpooled.directBuffer(STATIC_PLAINTEXT_LEN);
		tmpBuf.writeBytes(STATIC_PLAINTEXT);
		STATIC_PLAINTEXT_BYTEBUF = Unpooled.unreleasableBuffer(tmpBuf);
	}
	
	private static final CharSequence STATIC_PLAINTEXT_LEN_VALUE = AsciiString.cached(String.valueOf(STATIC_PLAINTEXT_LEN));
	
	private static class PlaintextSupplier implements Supplier<ByteBuf> {
		@Override
		public ByteBuf get() {
			return STATIC_PLAINTEXT_BYTEBUF.duplicate();
		}
	}
	
	private static final Mono<ByteBuf> PLAIN_TEXT_MONO = Mono.fromSupplier(new PlaintextSupplier());
	
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
		try {
			exchange.response()
				.headers(h -> h
					.add(HttpHeaderNames.SERVER, STATIC_SERVER)
					.add(HttpHeaderNames.DATE, this.date)
					.add(HttpHeaderNames.CONTENT_TYPE, HttpHeaderValues.APPLICATION_JSON)
				)
				.body()
					.raw()
						.value(Unpooled.unreleasableBuffer(Unpooled.wrappedBuffer(this.mapper.writeValueAsBytes(new Message("Hello, World!")))));
		} 
		catch (JsonProcessingException | IllegalStateException e) {
			throw new InternalServerErrorException("Error serializing message as JSON", e);
		}
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
				.raw().stream(this.sqlClient.get().flatMap(client -> 
					client.queryForObject(
						DB_SELECT_WORLD, 
						row -> {
							try {
								return Unpooled.unreleasableBuffer(Unpooled.wrappedBuffer(this.mapper.writeValueAsBytes(new World(row.getInteger(0), row.getInteger(1)))));
							} 
							catch (JsonProcessingException e) {
								throw new InternalServerErrorException(e);
							}
						}, 
						randomWorldId()
					)
				));
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
				.raw().stream(this.sqlClient.get()
					.flatMapMany(client -> ((UnsafeSqlOperations)client)
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
					.map(worlds -> {
						try {
							return Unpooled.unreleasableBuffer(Unpooled.wrappedBuffer(this.mapper.writeValueAsBytes(worlds)));
						} 
						catch (JsonProcessingException e) {
							throw new InternalServerErrorException(e);
						}
					})
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
			.raw().stream(this.sqlClient.get()
				.flatMapMany(client -> Flux.from(((UnsafeSqlOperations)client)
					.batchQueries(ops -> 
						Flux.range(0, queries)
							.map(ign -> ops.queryForObject(
								DB_SELECT_WORLD, 
								row -> new World(row.getInteger(0), randomWorldId()), 
								randomWorldId()
							))
					))
					.collectSortedList()
					.delayUntil(worlds -> client.batchUpdate(
							DB_UPDATE_WORLD, 
							worlds.stream().map(world -> new Object[] { world.getRandomNumber(), world.getId() })
						)
					)
					.map(worlds -> {
						try {
							return Unpooled.unreleasableBuffer(Unpooled.wrappedBuffer(this.mapper.writeValueAsBytes(worlds)));
						} 
						catch (JsonProcessingException e) {
							throw new InternalServerErrorException(e);
						}
					})
				)
			);
	}
	
	private static final CharSequence MEDIA_TEXT_HTML_UTF8 = AsciiString.cached("text/html; charset=utf-8");
	
	private static final FortunesTemplate.Renderer<CompletableFuture<ByteBuf>> FORTUNES_RENDERER = FortunesTemplate.bytebuf(() -> Unpooled.unreleasableBuffer(Unpooled.buffer()));
	
	public void handle_fortunes(Exchange<ExchangeContext> exchange) throws HttpException {
		exchange.response()
			.headers(h -> h
				.add(HttpHeaderNames.SERVER, STATIC_SERVER)
				.add(HttpHeaderNames.DATE, this.date)
				.add(HttpHeaderNames.CONTENT_TYPE, MEDIA_TEXT_HTML_UTF8)
			)
			.body()
				.raw().stream(this.sqlClient.get().flatMapMany(client -> 
					client.query(
							DB_SELECT_FORTUNE, 
							row -> new Fortune(row.getInteger(0), row.getString(1))
						)
					)
					.collectList()
					.flatMap(fortunes -> {
						fortunes.add(new Fortune(0, "Additional fortune added at request time."));
						Collections.sort(fortunes);
						return Mono.fromFuture(() -> FORTUNES_RENDERER.render(fortunes));
					})
				);
	}
}

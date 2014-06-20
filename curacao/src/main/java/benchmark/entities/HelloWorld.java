package benchmark.entities;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.annotations.SerializedName;
import com.kolich.curacao.gson.GsonAppendableCuracaoEntity;

public final class HelloWorld extends GsonAppendableCuracaoEntity {

    private static final Gson gson__ =
        new GsonBuilder().serializeNulls().create();

    @SerializedName("message")
    private final String message_;

    public HelloWorld(final String message) {
        super(gson__);
        message_ = message;
    }

}

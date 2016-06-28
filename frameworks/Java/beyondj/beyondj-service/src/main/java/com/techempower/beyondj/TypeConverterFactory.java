package com.techempower.beyondj;

import net.sourceforge.stripes.config.Configuration;
import net.sourceforge.stripes.integration.spring.SpringHelper;
import net.sourceforge.stripes.validation.DefaultTypeConverterFactory;
import net.sourceforge.stripes.validation.TypeConverter;
import org.springframework.web.context.ContextLoader;

import java.util.Locale;

public class TypeConverterFactory extends DefaultTypeConverterFactory {

	public void init(final Configuration configuration) {
		super.init(configuration);
	}

	@SuppressWarnings("rawtypes")
	@Override
	public TypeConverter getInstance(Class<? extends TypeConverter> clazz,
			Locale locale) throws Exception {
		TypeConverter converter = super.getInstance(clazz, locale);
		SpringHelper.injectBeans(converter,
				ContextLoader.getCurrentWebApplicationContext());
		return converter;
	}
}

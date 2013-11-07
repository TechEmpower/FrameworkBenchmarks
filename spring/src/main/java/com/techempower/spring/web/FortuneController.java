package com.techempower.spring.web;

import com.techempower.spring.domain.Fortune;
import com.techempower.spring.service.FortuneRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.ModelAndView;

import java.util.Collections;
import java.util.List;

@Controller
public class FortuneController {

	@Autowired
	private FortuneRepository fortuneRepository;

	@RequestMapping(value = "/fortunes")
	public ModelAndView fortunes() {

		List<Fortune> fortunes = fortuneRepository.findAll();
		fortunes.add(new Fortune(0, "Additional fortune added at request time."));
		Collections.sort(fortunes);

		return new ModelAndView("fortunes", "fortunes", fortunes);
	}
}

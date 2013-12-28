package com.techempower.spring.web;

import com.techempower.spring.domain.Fortune;
import com.techempower.spring.repository.FortuneRepository;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.ui.ModelMap;
import org.springframework.web.bind.annotation.RequestMapping;

import java.util.Collections;
import java.util.List;

@Controller
final class FortuneController {

    @Autowired
    private FortuneRepository fortuneRepository;

    @RequestMapping(value = "/fortunes")
     String fortunes(ModelMap modelMap) {
        List<Fortune> fortunes = this.fortuneRepository.findAll();
        fortunes.add(new Fortune(0, "Additional fortune added at request time."));
        Collections.sort(fortunes);

        modelMap.addAttribute("fortunes", fortunes);
        return "fortunes";
    }
}

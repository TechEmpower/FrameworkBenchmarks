package com.example.helloworld.db;

import com.example.helloworld.db.model.Fortune;

import java.util.List;

public interface FortuneDAO  {
    List<Fortune> list();
}

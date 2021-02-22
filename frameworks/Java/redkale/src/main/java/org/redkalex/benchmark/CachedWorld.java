/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkalex.benchmark;

import javax.persistence.*;

/**
 *
 * @author zhangjx
 */
@Cacheable(direct=true)
@Table(name = "World")
public class CachedWorld extends World {

}

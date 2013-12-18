package hello.controllers;

import com.google.inject.Inject;
import com.google.inject.persist.UnitOfWork;
import ninja.Context;
import ninja.Filter;
import ninja.FilterChain;
import ninja.Result;

/**
 *
 * @author ra
 */
public class DatabaseAccess implements Filter {
    
    @Inject
    private UnitOfWork unitOfWork;

    @Override
    public Result filter(FilterChain filterChain, Context context) {
        
        unitOfWork.begin();
        Result result = filterChain.next(context);
        unitOfWork.end();
        
        return result;
     }
 
}



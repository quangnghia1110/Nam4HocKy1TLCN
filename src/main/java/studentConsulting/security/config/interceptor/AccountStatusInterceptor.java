package studentConsulting.security.config.interceptor;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;
import org.springframework.web.servlet.HandlerInterceptor;
import studentConsulting.model.entity.AccountEntity;
import studentConsulting.model.exception.Exceptions;
import studentConsulting.repository.admin.AccountRepository;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

@Component
public class AccountStatusInterceptor implements HandlerInterceptor {

    @Autowired
    private AccountRepository accountRepository;

//    @Override
//    public boolean preHandle(HttpServletRequest request, HttpServletResponse response, Object handler) throws Exception {
//        String email = SecurityContextHolder.getContext().getAuthentication().getName();
//
//        AccountEntity account = accountRepository.findByEmail(email)
//                .orElseThrow(() -> new Exceptions.ErrorException("Tài khoản không tồn tại"));
//
//        if (!account.isActivity()) {
//            response.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
//            response.getWriter().write("Tài khoản của bạn đã bị khóa. Vui lòng đăng nhập lại.");
//            return false;
//        }
//
//        return true;
//    }
}


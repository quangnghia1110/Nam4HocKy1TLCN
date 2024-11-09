package studentConsulting.service.implement.common;

import lombok.Getter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import studentConsulting.model.entity.AccountEntity;
import studentConsulting.model.exception.Exceptions;
import studentConsulting.repository.admin.AccountRepository;
import studentConsulting.service.interfaces.common.IStatusOnlineService;

import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

@Service
public class StatusOnlineServiceImpl implements IStatusOnlineService {
    //ConcurrentHashMap cho phép truy cập đồng thời từ nhiều luồng
    @Getter
    private final Map<String, LocalDateTime> onlineUsers = new ConcurrentHashMap<>();

    @Autowired
    private AccountRepository accountRepository;

    @Override
    public void updateStatus(String email, boolean isOnline) {
        AccountEntity account = accountRepository.findByEmail(email)
                .orElseThrow(() -> new Exceptions.ErrorException("Người dùng không được tìm thấy với email"));

        if (isOnline) {
            account.setLastActivity(LocalDateTime.now());
            account.setIsOnline(true);
            accountRepository.save(account);
            onlineUsers.put(email, LocalDateTime.now());
        } else {
            account.setIsOnline(false);
            account.setLastActivity(LocalDateTime.now());
            accountRepository.save(account);
            onlineUsers.remove(email);
        }
    }

}

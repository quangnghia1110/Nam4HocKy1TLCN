package studentConsulting.service.implement.common;

import lombok.Getter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import studentConsulting.model.entity.authentication.AccountEntity;
import studentConsulting.model.exception.Exceptions;
import studentConsulting.repository.authentication.AccountRepository;

import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

@Service
public class StatusOnlineService {
    @Getter
    private final Map<String, LocalDateTime> onlineUsers = new HashMap<>();
    @Autowired
    private AccountRepository accountRepository;

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

    @Scheduled(fixedRate = 300000)
    public void checkForInactiveUsers() {
        Iterator<Map.Entry<String, LocalDateTime>> iterator = onlineUsers.entrySet().iterator();
        LocalDateTime now = LocalDateTime.now();

        while (iterator.hasNext()) {
            Map.Entry<String, LocalDateTime> entry = iterator.next();
            LocalDateTime lastActiveTime = entry.getValue();

            long secondsInactive = ChronoUnit.SECONDS.between(lastActiveTime, now);

            if (secondsInactive >= 300) {
                AccountEntity account = accountRepository.findByEmail(entry.getKey())
                        .orElseThrow(() -> new Exceptions.ErrorException("Người dùng không được tìm thấy với email " + entry.getKey()));

                account.setIsOnline(false);
                accountRepository.save(account);

                iterator.remove();
                System.out.println("Người dùng " + entry.getKey() + " đã bị đánh dấu là offline do không hoạt động.");
            }
        }
    }
}

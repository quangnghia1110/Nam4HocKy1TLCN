package studentConsulting.controller.common;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.event.EventListener;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.messaging.simp.stomp.StompHeaderAccessor;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.messaging.SessionConnectEvent;
import org.springframework.web.socket.messaging.SessionDisconnectEvent;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.model.entity.AccountEntity;
import studentConsulting.model.exception.Exceptions;
import studentConsulting.model.payload.dto.actor.UserOnlineDTO;
import studentConsulting.repository.admin.AccountRepository;
import studentConsulting.repository.admin.UserRepository;
import studentConsulting.security.jwt.JwtProvider;
import studentConsulting.service.interfaces.common.IStatusOnlineService;

import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.util.List;
import java.util.stream.Collectors;

@Component
public class StatusOnlineController {
    @Autowired
    private IStatusOnlineService commonStatusOnlineService;

    @Autowired
    private SimpMessagingTemplate messagingTemplate;

    @Autowired
    private AccountRepository accountRepository;

    @Autowired
    private JwtProvider jwtProvider;

    @Autowired
    private UserRepository userRepository;

    @EventListener
    public void handleWebSocketConnectListener(SessionConnectEvent event) {
        System.out.println("Kết nối WebSocket được kích hoạt");

        StompHeaderAccessor headerAccessor = StompHeaderAccessor.wrap(event.getMessage());
        String token = headerAccessor.getFirstNativeHeader("Authorization");

        if (token != null && token.startsWith("Bearer ")) {
            token = token.substring(7);
            String email = jwtProvider.getEmailFromToken(token);

            commonStatusOnlineService.updateStatus(email, true);
            AccountEntity account = accountRepository.findByEmail(email)
                    .orElseThrow(() -> new Exceptions.ErrorException("Người dùng không được tìm thấy với email: " + email));

            System.out.println("Test1: " + email);
            System.out.println("Online1: " + account.getIsOnline());
            sendOnlineUsersUpdate();
        } else {
            System.out.println("Authorization token is missing or invalid.");
        }
    }

    @EventListener
    public void handleWebSocketDisconnectListener(SessionDisconnectEvent event) {
        System.out.println("Ngắt kết nối WebSocket được kích hoạt");

        StompHeaderAccessor headerAccessor = StompHeaderAccessor.wrap(event.getMessage());
        String token = headerAccessor.getFirstNativeHeader("Authorization");

        if (token != null && token.startsWith("Bearer ")) {
            token = token.substring(7);
            String email = jwtProvider.getEmailFromToken(token);

            commonStatusOnlineService.updateStatus(email, false);
            AccountEntity account = accountRepository.findByEmail(email)
                    .orElseThrow(() -> new Exceptions.ErrorException("Người dùng không được tìm thấy với email: " + email));

            System.out.println("Test2: " + email);
            System.out.println("Online2: " + account.getIsOnline());
            sendOnlineUsersUpdate();
        } else {
            System.out.println("Authorization token is missing or invalid.");
        }
    }

    @Scheduled(fixedRate = 300000)
    public void checkAndUpdateOnlineStatus() {
        LocalDateTime now = LocalDateTime.now();

        commonStatusOnlineService.getOnlineUsers().forEach((email, lastActiveTime) -> {
            long secondsInactive = ChronoUnit.SECONDS.between(lastActiveTime, now);
            if (secondsInactive >= 300) {
                commonStatusOnlineService.updateStatus(email, false);

                AccountEntity account = accountRepository.findByEmail(email)
                        .orElseThrow(() -> new Exceptions.ErrorException("Người dùng không được tìm thấy với email: " + email));

                System.out.println("Test3: " + email);
                System.out.println("Online3: " + account.getIsOnline());
            }
        });

        sendOnlineUsersUpdate();
    }

    private void sendOnlineUsersUpdate() {
        List<UserOnlineDTO> onlineUsers = getOnlineUsers();
        List<UserOnlineDTO> allUsers = getAllUsers();

        for (UserOnlineDTO receiver : allUsers) {
            String destination = "/user/" + receiver.getId() + "/online-users";
            messagingTemplate.convertAndSend(destination, onlineUsers);
        }
    }

    public List<UserOnlineDTO> getAllUsers() {
        return userRepository.findAll()
                .stream()
                .map(user -> new UserOnlineDTO(
                        user.getId(),
                        user.getName(),
                        user.getAccount().getEmail(),
                        user.getPhone(),
                        "Online",
                        user.getAvatarUrl()
                ))
                .collect(Collectors.toList());
    }

    private List<UserOnlineDTO> getOnlineUsers() {
        LocalDateTime now = LocalDateTime.now();

        return commonStatusOnlineService.getOnlineUsers().entrySet().stream()
                .filter(entry -> {
                    long secondsInactive = ChronoUnit.SECONDS.between(entry.getValue(), now);
                    return secondsInactive < 300;
                })
                .map(entry -> {
                    String email = entry.getKey();
                    AccountEntity account = accountRepository.findByEmail(email)
                            .orElseThrow(() -> new Exceptions.ErrorException("Người dùng không được tìm thấy với email1: " + email));

                    String role = SecurityConstants.Role.TUVANVIEN;
                    if (account.getRole() != null && role.equals(account.getRole().getName())) {
                        return new UserOnlineDTO(
                                account.getId(),
                                account.getName(),
                                account.getEmail(),
                                account.getPhone(),
                                "Online",
                                account.getUserInformation().getAvatarUrl()
                        );
                    }
                    return null;
                })
                .filter(userOnlineDTO -> userOnlineDTO != null)
                .collect(Collectors.toList());
    }
}

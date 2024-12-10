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
        StompHeaderAccessor headerAccessor = StompHeaderAccessor.wrap(event.getMessage());
        String token = headerAccessor.getFirstNativeHeader("Authorization");

        if (token != null && token.startsWith("Bearer ")) {
            token = token.substring(7);
            String email = jwtProvider.getEmailFromToken(token);

            commonStatusOnlineService.updateStatus(email, true);
            accountRepository.findByEmail(email).ifPresentOrElse(account -> {

                sendOnlineUsersUpdate();
            }, () -> System.out.println("Người dùng không được tìm thấy với email: " + email));
        } else {
            System.out.println("Authorization token is missing or invalid.");
        }
    }


    @EventListener
    public void handleWebSocketDisconnectListener(SessionDisconnectEvent event) {

        StompHeaderAccessor headerAccessor = StompHeaderAccessor.wrap(event.getMessage());
        String token = headerAccessor.getFirstNativeHeader("Authorization");

        if (token != null && token.startsWith("Bearer ")) {
            token = token.substring(7);
            String email = jwtProvider.getEmailFromToken(token);

            commonStatusOnlineService.updateStatus(email, false);
            accountRepository.findByEmail(email).ifPresentOrElse(account -> {
                sendOnlineUsersUpdate();
            }, () -> System.out.println("Người dùng không được tìm thấy với email: " + email));
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

                accountRepository.findByEmail(email).ifPresent(account -> {
                });
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
                    return accountRepository.findByEmail(email).map(account -> {
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
                    }).orElse(null);
                })
                .filter(userOnlineDTO -> userOnlineDTO != null)
                .collect(Collectors.toList());
    }

}

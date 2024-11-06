package studentConsulting.controller.common;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.event.EventListener;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.messaging.simp.stomp.StompHeaderAccessor;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.messaging.SessionConnectEvent;
import org.springframework.web.socket.messaging.SessionDisconnectEvent;
import studentConsulting.model.entity.AccountEntity;
import studentConsulting.model.exception.Exceptions;
import studentConsulting.model.payload.dto.actor.UserOnlineDTO;
import studentConsulting.repository.admin.AccountRepository;
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

    @EventListener
    public void handleWebSocketConnectListener(SessionConnectEvent event) {
        StompHeaderAccessor headerAccessor = StompHeaderAccessor.wrap(event.getMessage());

        String token = headerAccessor.getFirstNativeHeader("Authorization");

        if (token != null && token.startsWith("Bearer ")) {
            token = token.substring(7);
            String email = jwtProvider.getEmailFromToken(token);

            commonStatusOnlineService.updateStatus(email, true);

            sendOnlineUsersUpdate();
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

            sendOnlineUsersUpdate();
        } else {
            System.out.println("Authorization token is missing or invalid.");
        }
    }


    private void sendOnlineUsersUpdate() {
        List<UserOnlineDTO> onlineUsers = getOnlineUsers();
        messagingTemplate.convertAndSend("/user/online-users", onlineUsers);
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

                    if (account.getRole() != null && "ROLE_TUVANVIEN".equals(account.getRole().getName())) {
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

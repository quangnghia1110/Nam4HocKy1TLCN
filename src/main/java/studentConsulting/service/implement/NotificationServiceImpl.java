package studentConsulting.service.implement;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import studentConsulting.model.entity.notification.NotificationEntity;
import studentConsulting.repository.NotificationRepository;
import studentConsulting.service.INotificationService;

import java.time.LocalDate;
import java.util.List;

@Service
public class NotificationServiceImpl implements INotificationService {

    private final SimpMessagingTemplate messagingTemplate;
    private final NotificationRepository notificationRepository;

    @Autowired
    public NotificationServiceImpl(SimpMessagingTemplate messagingTemplate, NotificationRepository notificationRepository) {
        this.messagingTemplate = messagingTemplate;
        this.notificationRepository = notificationRepository;
    }
    
    @Override
    public void sendNotification(NotificationEntity notification) {
        notification.setTime(LocalDate.now());
        notificationRepository.save(notification);

        messagingTemplate.convertAndSendToUser(notification.getReceiverId().toString(), "/queue/notifications", notification);
    }
    
    @Override
    public List<NotificationEntity> getNotificationsByReceiverId(Integer receiverId) {
        return notificationRepository.findByReceiverId(receiverId);
    }
}


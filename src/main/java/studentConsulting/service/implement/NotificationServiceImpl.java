package studentConsulting.service.implement;

import java.time.LocalDateTime;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Service;

import studentConsulting.model.entity.notification.NotificationEntity;
import studentConsulting.repository.NotificationRepository;
import studentConsulting.service.INotificationService;

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
        notification.setTime(LocalDateTime.now());
        notificationRepository.save(notification);

        messagingTemplate.convertAndSendToUser(notification.getReceiverId().toString(), "/queue/notifications", notification);
    }
    
    @Override
    public List<NotificationEntity> getNotificationsByReceiverId(Integer receiverId) {
        return notificationRepository.findByReceiverId(receiverId);
    }
}


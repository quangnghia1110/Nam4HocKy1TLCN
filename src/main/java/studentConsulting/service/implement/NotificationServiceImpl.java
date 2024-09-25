package studentConsulting.service.implement;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import studentConsulting.constant.enums.NotificationStatus;
import studentConsulting.constant.enums.NotificationType;
import studentConsulting.model.entity.notification.NotificationEntity;
import studentConsulting.model.payload.dto.NotificationResponseDTO.NotificationDTO;
import studentConsulting.repository.NotificationRepository;
import studentConsulting.service.INotificationService;

@Service
public class NotificationServiceImpl implements INotificationService {

    private final NotificationRepository notificationRepository;

    @Autowired
    public NotificationServiceImpl(NotificationRepository notificationRepository) {
        this.notificationRepository = notificationRepository;
    }

    @Override
    public void sendNotification(NotificationDTO notificationDTO) {
        NotificationEntity notificationEntity = NotificationEntity.builder()
                .senderId(notificationDTO.getSenderId())
                .receiverId(notificationDTO.getReceiverId())
                .content(notificationDTO.getContent())
                .time(notificationDTO.getTime())
                .notificationType(NotificationType.valueOf(notificationDTO.getNotificationType())) // Convert from string to enum
                .status(NotificationStatus.valueOf(notificationDTO.getStatus())) // Convert from string to enum
                .build();

        notificationRepository.save(notificationEntity);
    }

    @Override
    public List<NotificationEntity> getNotificationsByReceiverId(Integer receiverId) {
        return notificationRepository.findByReceiverId(receiverId);
    }
}

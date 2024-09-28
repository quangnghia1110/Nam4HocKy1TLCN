package studentConsulting.service.implement.common;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import studentConsulting.constant.enums.NotificationStatus;
import studentConsulting.constant.enums.NotificationType;
import studentConsulting.model.entity.notification.NotificationEntity;
import studentConsulting.model.payload.dto.NotificationResponseDTO.NotificationDTO;
import studentConsulting.repository.common.NotificationRepository;
import studentConsulting.service.interfaces.common.ICommonNotificationService;

import java.util.List;

@Service
public class CommonNotificationServiceImpl implements ICommonNotificationService {

    private final NotificationRepository notificationRepository;

    @Autowired
    public CommonNotificationServiceImpl(NotificationRepository notificationRepository) {
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

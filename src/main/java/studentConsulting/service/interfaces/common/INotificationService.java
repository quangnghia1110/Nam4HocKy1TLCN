package studentConsulting.service.interfaces.common;

import studentConsulting.model.entity.notification.NotificationEntity;
import studentConsulting.model.payload.dto.notification.NotificationResponseDTO.NotificationDTO;

import java.util.List;

public interface INotificationService {
    void sendNotification(NotificationDTO notificationDTO);

    List<NotificationEntity> getNotificationsByReceiverId(Integer receiverId);

//    Page<studentConsulting.model.payload.dto.notification.NotificationDTO> findNotificationsByUserWithFilters(Integer userId, String content, LocalDate startDate, LocalDate endDate, Pageable pageable);

}

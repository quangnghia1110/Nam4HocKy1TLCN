package studentConsulting.repository.common;

import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;
import studentConsulting.model.entity.NotificationEntity;

import java.util.List;

@Repository
public interface NotificationRepository extends PagingAndSortingRepository<NotificationEntity, Integer>, JpaSpecificationExecutor<NotificationEntity> {

    List<NotificationEntity> findByReceiverId(Integer receiverId);

}

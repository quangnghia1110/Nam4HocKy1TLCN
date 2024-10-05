package studentConsulting.repository.communication;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;
import studentConsulting.model.entity.communication.MessageRecallEntity;

@Repository
public interface MessageRecallRepository extends PagingAndSortingRepository<MessageRecallEntity, Integer>, JpaSpecificationExecutor<MessageRecallEntity>, JpaRepository<MessageRecallEntity, Integer> {
    boolean existsByMessageIdAndUserId(Integer messageId, Integer userId);

}

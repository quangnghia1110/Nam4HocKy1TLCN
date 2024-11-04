package studentConsulting.repository.actor;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import studentConsulting.model.entity.MessageEntity;

import java.util.List;

@Repository
public interface MessageRepository extends PagingAndSortingRepository<MessageEntity, Integer>, JpaSpecificationExecutor<MessageEntity>, JpaRepository<MessageEntity, Integer> {
    List<MessageEntity> findByConversationId(Integer conversationId);

    @Modifying
    @Query("DELETE FROM MessageEntity m WHERE m.conversationId = :conversationId")
    void deleteMessagesByConversationId(@Param("conversationId") Integer conversationId);

}

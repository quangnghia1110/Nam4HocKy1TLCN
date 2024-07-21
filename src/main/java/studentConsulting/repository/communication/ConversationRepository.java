package studentConsulting.repository.communication;

import org.springframework.data.jpa.repository.JpaRepository;

import studentConsulting.model.entity.communication.ConversationEntity;

public interface ConversationRepository extends  JpaRepository<ConversationEntity, Integer>{

}

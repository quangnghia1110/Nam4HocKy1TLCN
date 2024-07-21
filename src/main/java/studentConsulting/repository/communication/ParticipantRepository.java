package studentConsulting.repository.communication;

import org.springframework.data.jpa.repository.JpaRepository;

import studentConsulting.model.entity.communication.ParticipantEntity;

public interface ParticipantRepository extends  JpaRepository<ParticipantEntity, Integer>{

}

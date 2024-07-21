package studentConsulting.repository.consultation;

import org.springframework.data.jpa.repository.JpaRepository;

import studentConsulting.model.entity.consultation.ConsultationScheduleEntity;

public interface ConsultationScheduleRepository extends  JpaRepository<ConsultationScheduleEntity, Integer>{

}

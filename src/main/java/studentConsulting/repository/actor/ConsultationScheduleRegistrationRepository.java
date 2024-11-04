package studentConsulting.repository.actor;

import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;
import studentConsulting.model.entity.ConsultationScheduleEntity;
import studentConsulting.model.entity.ConsultationScheduleRegistrationEntity;
import studentConsulting.model.entity.UserInformationEntity;

import java.util.Optional;

@Repository
public interface ConsultationScheduleRegistrationRepository extends PagingAndSortingRepository<ConsultationScheduleRegistrationEntity, Integer>, JpaSpecificationExecutor<ConsultationScheduleRegistrationEntity> {
    boolean existsByUserAndConsultationSchedule(UserInformationEntity user, ConsultationScheduleEntity consultationSchedule);

    Optional<ConsultationScheduleRegistrationEntity> findByUserAndConsultationSchedule(UserInformationEntity user, ConsultationScheduleEntity consultationSchedule);

}
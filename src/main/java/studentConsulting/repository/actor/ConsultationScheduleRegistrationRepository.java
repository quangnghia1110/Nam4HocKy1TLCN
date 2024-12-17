package studentConsulting.repository.actor;

import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;
import studentConsulting.model.entity.ConsultationScheduleEntity;
import studentConsulting.model.entity.ConsultationScheduleRegistrationEntity;
import studentConsulting.model.entity.UserInformationEntity;

import java.util.List;
import java.util.Optional;

@Repository
public interface ConsultationScheduleRegistrationRepository extends PagingAndSortingRepository<ConsultationScheduleRegistrationEntity, Integer>, JpaSpecificationExecutor<ConsultationScheduleRegistrationEntity> {
    boolean existsByUserAndConsultationSchedule(UserInformationEntity user, ConsultationScheduleEntity consultationSchedule);

    Optional<ConsultationScheduleRegistrationEntity> findByUserAndConsultationSchedule(UserInformationEntity user, ConsultationScheduleEntity consultationSchedule);

    @Modifying
    @Query("DELETE FROM ConsultationScheduleRegistrationEntity c WHERE c.consultationSchedule.id = :scheduleId")
    @Transactional
    void deleteByScheduleId(@Param("scheduleId") Integer scheduleId);

    List<ConsultationScheduleRegistrationEntity> findByConsultationSchedule(ConsultationScheduleEntity consultationSchedule);
}
package studentConsulting.service.interfaces.consultant;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import studentConsulting.model.entity.consultation_schedule.ConsultationScheduleEntity;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.payload.dto.ConsultationScheduleDTO;
import studentConsulting.model.payload.dto.ManageConsultantScheduleDTO;
import studentConsulting.model.payload.request.consultant.ConsultationFeedbackRequest;
import studentConsulting.model.payload.request.consultant.UpdateConsultationScheduleRequest;

import java.time.LocalDate;
import java.util.Optional;

public interface IConsultantConsultationScheduleService {

    Page<ConsultationScheduleDTO> getConsultationsByConsultantWithFilters(
            UserInformationEntity consultant, String title, Boolean statusPublic, Boolean statusConfirmed, Boolean mode,
            LocalDate startDate, LocalDate endDate, Pageable pageable);

    void confirmConsultationSchedule(Integer scheduleId, ConsultationFeedbackRequest request,
                                     UserInformationEntity consultant);

    Optional<ConsultationScheduleEntity> findConsulationScheduleById(Integer scheduleId);

    ManageConsultantScheduleDTO updateConsultationSchedule(Integer scheduleId, Integer departmentId, UpdateConsultationScheduleRequest request);
}


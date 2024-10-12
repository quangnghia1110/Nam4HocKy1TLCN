package studentConsulting.service.interfaces.consultant;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.payload.dto.consultation_schedule.ConsultationScheduleDTO;
import studentConsulting.model.payload.dto.consultation_schedule.ManageConsultantScheduleDTO;
import studentConsulting.model.payload.request.consultant.UpdateConsultationScheduleRequest;

import java.security.Principal;
import java.time.LocalDate;

public interface IConsultantConsultationScheduleService {

    Page<ConsultationScheduleDTO> getConsultationsByConsultantWithFilters(
            UserInformationEntity consultant, String title, Boolean statusPublic, Boolean statusConfirmed, Boolean mode,
            LocalDate startDate, LocalDate endDate, Pageable pageable);

    ManageConsultantScheduleDTO confirmConsultationSchedule(Integer scheduleId, Integer departmentId, UpdateConsultationScheduleRequest request);

    ManageConsultantScheduleDTO getConsultationScheduleDetail(Integer scheduleId, Principal principal);

}


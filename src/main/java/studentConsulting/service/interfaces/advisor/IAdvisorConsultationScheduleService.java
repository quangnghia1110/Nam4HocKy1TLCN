package studentConsulting.service.interfaces.advisor;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import studentConsulting.model.payload.dto.consultation_schedule.ConsultationScheduleDTO;
import studentConsulting.model.payload.dto.consultation_schedule.ConsultationScheduleRegistrationMemberDTO;
import studentConsulting.model.payload.dto.consultation_schedule.ManageConsultantScheduleDTO;
import studentConsulting.model.payload.request.consultant.ManageCreateConsultantScheduleRequest;
import studentConsulting.model.payload.request.consultant.UpdateConsultationScheduleRequest;

import java.security.Principal;
import java.time.LocalDate;
import java.util.List;

public interface IAdvisorConsultationScheduleService {

    ConsultationScheduleDTO createConsultationSchedule(ManageCreateConsultantScheduleRequest request, Integer departmentId, Integer userId);

    ConsultationScheduleDTO updateConsultationSchedule(Integer scheduleId, Integer departmentId, boolean isAdmin, UpdateConsultationScheduleRequest request, String role, Integer userId);

    Page<ConsultationScheduleRegistrationMemberDTO> getMembersByConsultationSchedule(Integer consultationScheduleId, LocalDate startDate, LocalDate endDate, Pageable pageable, Integer userId);

    void deleteConsultationSchedule(Integer scheduleId, Integer departmentId, Integer userId, String role);

    Page<ConsultationScheduleDTO> getAllConsultationSchedulesWithFilters(String title, Boolean statusPublic, Boolean statusConfirmed, Boolean mode, LocalDate startDate, LocalDate endDate, Pageable pageable);

    Page<ManageConsultantScheduleDTO> getAllConsultationsWithFilters(String title, Boolean statusPublic, Boolean statusConfirmed, Boolean mode, LocalDate startDate, LocalDate endDate, Pageable pageable);

    void importConsultationSchedules(List<List<String>> csvData);

    void importManageConsultantSchedules(List<List<String>> csvData);

    ConsultationScheduleDTO getConsultationScheduleByRole(Integer scheduleId, String role, Integer departmentId, Integer userId);
}


package studentConsulting.service.interfaces.actor;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.payload.dto.consultation_schedule.ConsultationScheduleDTO;
import studentConsulting.model.payload.dto.consultation_schedule.ConsultationScheduleRegistrationDTO;
import studentConsulting.model.payload.dto.consultation_schedule.ConsultationScheduleRegistrationMemberDTO;
import studentConsulting.model.payload.dto.consultation_schedule.ManageConsultantScheduleDTO;
import studentConsulting.model.payload.request.consultant.ConsultationScheduleRegistrationRequest;
import studentConsulting.model.payload.request.consultant.CreateScheduleConsultationRequest;
import studentConsulting.model.payload.request.consultant.ManageCreateConsultantScheduleRequest;
import studentConsulting.model.payload.request.consultant.UpdateConsultationScheduleRequest;

import java.time.LocalDate;
import java.util.List;

public interface IConsultationScheduleService {
    ConsultationScheduleDTO createConsultation(CreateScheduleConsultationRequest request, UserInformationEntity user);

    Page<ConsultationScheduleDTO> getConsultationScheduleByRole(UserInformationEntity user, String title, Boolean statusPublic, Boolean statusConfirmed, Boolean mode, LocalDate startDate, LocalDate endDate, Pageable pageable);

    ConsultationScheduleRegistrationDTO registerForConsultation(ConsultationScheduleRegistrationRequest request, UserInformationEntity user);

    Page<ConsultationScheduleRegistrationDTO> getSchedulesJoinByUser(UserInformationEntity user, LocalDate startDate, LocalDate endDate, Pageable pageable);

    void cancelRegistrationForConsultation(Integer id, UserInformationEntity user);


    ManageConsultantScheduleDTO confirmConsultationSchedule(Integer scheduleId, Integer departmentId, UpdateConsultationScheduleRequest request);


    ConsultationScheduleDTO createConsultationSchedule(ManageCreateConsultantScheduleRequest request, Integer departmentId, Integer userId);

    ConsultationScheduleDTO updateConsultationSchedule(Integer scheduleId, Integer departmentId, boolean isAdmin, UpdateConsultationScheduleRequest request, String role, Integer userId);

    Page<ConsultationScheduleRegistrationMemberDTO> getMembersByConsultationSchedule(Integer consultationScheduleId, LocalDate startDate, LocalDate endDate, Pageable pageable, Integer userId);

    void deleteConsultationSchedule(Integer scheduleId, Integer departmentId, Integer userId, String role);

    Page<ConsultationScheduleDTO> getAllConsultationSchedulesWithFilters(String title, Boolean statusPublic, Boolean statusConfirmed, Boolean mode, LocalDate startDate, LocalDate endDate, Pageable pageable);

    ConsultationScheduleDTO getConsultationScheduleByRole(Integer scheduleId, String role, Integer departmentId, Integer userId);

    void importManageConsultantSchedules(List<List<String>> csvData);
}


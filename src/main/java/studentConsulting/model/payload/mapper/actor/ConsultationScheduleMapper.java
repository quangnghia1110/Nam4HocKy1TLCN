package studentConsulting.model.payload.mapper.actor;

import org.springframework.stereotype.Component;
import studentConsulting.model.entity.ConsultationScheduleEntity;
import studentConsulting.model.payload.dto.actor.ConsultationScheduleDTO;
import studentConsulting.model.payload.dto.actor.DepartmentDTO;
import studentConsulting.model.payload.dto.manage.ManageConsultantScheduleDTO;

@Component
public class ConsultationScheduleMapper {
    public ConsultationScheduleDTO mapToDTO(ConsultationScheduleEntity schedule) {
        return ConsultationScheduleDTO.builder()
                .id(schedule.getId())
                .department(schedule.getDepartment() != null
                        ? new DepartmentDTO(
                        schedule.getDepartment().getId(),
                        schedule.getDepartment().getName()
                )
                        : null)
                .title(schedule.getTitle())
                .content(schedule.getContent())
                .consultationDate(schedule.getConsultationDate())
                .consultationTime(schedule.getConsultationTime())
                .location(schedule.getLocation())
                .link(schedule.getLink())
                .mode(schedule.getMode())
                .statusPublic(schedule.getStatusPublic())
                .statusConfirmed(schedule.getStatusConfirmed())
                .consultantName(schedule.getConsultant().getLastName() + " " + schedule.getConsultant().getFirstName())
                .userName(schedule.getUser().getLastName() + " " + schedule.getUser().getFirstName())
                .createdBy(schedule.getUser().getId())
                .build();
    }

    public ManageConsultantScheduleDTO mapToDTOs(ConsultationScheduleEntity schedule) {
        return ManageConsultantScheduleDTO.builder()
                .id(schedule.getId())
                .title(schedule.getTitle())
                .content(schedule.getContent())
                .consultationDate(schedule.getConsultationDate())
                .consultationTime(schedule.getConsultationTime())
                .location(schedule.getLocation())
                .link(schedule.getLink())
                .mode(schedule.getMode())
                .statusPublic(schedule.getStatusPublic())
                .statusConfirmed(schedule.getStatusConfirmed())
                .created_by(schedule.getCreatedBy())
                .build();
    }
}

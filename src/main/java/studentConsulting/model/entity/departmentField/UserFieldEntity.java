package studentConsulting.model.entity.departmentField;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import studentConsulting.model.entity.authentication.UserInformationEntity;

import java.sql.Timestamp;
import java.time.LocalDate;

@Data
@Builder
@Entity
@Table(name = "user_fields")
@NoArgsConstructor
@AllArgsConstructor
public class UserFieldEntity {
	@Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column( name = "id")
    private Integer id;

    @Column(name = "created_at",  updatable = false)
    private LocalDate createdAt;

    @ManyToOne
    @JoinColumn(name = "user_id",  referencedColumnName = "id")
    private UserInformationEntity user; 

    @ManyToOne
    @JoinColumn(name = "field_id",  referencedColumnName = "id")
    private FieldEntity field;
}


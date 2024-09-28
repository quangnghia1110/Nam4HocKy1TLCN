package studentConsulting.model.entity.news;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.EmbeddedId;
import javax.persistence.Entity;
import javax.persistence.Table;

@Entity
@Table(name = "like_record")
@Data
@AllArgsConstructor
@NoArgsConstructor
public class LikeRecord {
    @EmbeddedId
    private LikeKey likeKey;
}
